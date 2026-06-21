# VisionSimR Shiny Dashboard
# Launch: shiny::runApp(system.file("shiny", package = "VisionSimR"))

library(shiny)
library(bslib)
library(bsicons)
library(imager)
library(shinycssloaders)
library(VisionSimR)

# Custom CSS for dark-mode clinical dashboard look
custom_css <- '
@import url("https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800;900&family=JetBrains+Mono:wght@400;500;600&display=swap");

body {
  font-family: "Inter", sans-serif !important;
  background: #030712 !important;
  overflow-x: hidden;
  font-size: 13px !important;
}

body::before {
  content: "";
  position: fixed;
  inset: 0;
  background:
    radial-gradient(ellipse 80% 60% at 10% 20%, rgba(0,240,255,0.06) 0%, transparent 60%),
    radial-gradient(ellipse 60% 80% at 90% 80%, rgba(168,85,247,0.05) 0%, transparent 60%),
    radial-gradient(ellipse 50% 50% at 50% 50%, rgba(0,240,255,0.02) 0%, transparent 70%);
  pointer-events: none;
  z-index: 0;
  animation: meshDrift 20s ease-in-out infinite alternate;
}

@keyframes meshDrift {
  0%   { opacity: 0.6; transform: scale(1) translate(0, 0); }
  50%  { opacity: 1;   transform: scale(1.05) translate(-1%, 2%); }
  100% { opacity: 0.7; transform: scale(0.98) translate(1%, -1%); }
}

.navbar {
  background: rgba(3,7,18,0.85) !important;
  backdrop-filter: blur(24px) saturate(1.8) !important;
  -webkit-backdrop-filter: blur(24px) saturate(1.8) !important;
  border-bottom: 1px solid rgba(0,240,255,0.1) !important;
  padding: 0.25rem 1rem !important;
  min-height: auto !important;
  z-index: 1000;
}

.navbar-brand {
  font-weight: 800 !important;
  font-size: 1rem !important;
  letter-spacing: -0.02em !important;
  background: linear-gradient(135deg, #00F0FF 0%, #A855F7 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}

.nav-link {
  color: #9CA3AF !important;
  font-weight: 500 !important;
  font-size: 0.7rem !important;
  letter-spacing: 0.03em !important;
  text-transform: uppercase !important;
  padding: 0.3rem 0.8rem !important;
  border-radius: 6px !important;
  transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1) !important;
  position: relative;
}

.nav-link:hover, .nav-link.active {
  color: #00F0FF !important;
  background: rgba(0,240,255,0.06) !important;
}

.nav-link.active::after {
  content: "";
  position: absolute;
  bottom: -2px;
  left: 20%;
  width: 60%;
  height: 2px;
  background: linear-gradient(90deg, #00F0FF, #A855F7);
  border-radius: 2px;
  box-shadow: 0 0 10px rgba(0,240,255,0.5);
}

.bslib-sidebar-layout > .sidebar {
  background: rgba(17,24,39,0.65) !important;
  backdrop-filter: blur(20px) !important;
  -webkit-backdrop-filter: blur(20px) !important;
  border-right: 1px solid rgba(0,240,255,0.08) !important;
  border-radius: 0 !important;
  font-size: 12px !important;
}

.glass-card {
  background: rgba(17,24,39,0.6);
  backdrop-filter: blur(16px) saturate(1.6);
  -webkit-backdrop-filter: blur(16px) saturate(1.6);
  border: 1px solid rgba(0,240,255,0.08);
  border-radius: 12px;
  padding: 0.8rem;
  transition: border-color 0.3s ease, box-shadow 0.3s ease;
}

.glass-card:hover {
  border-color: rgba(0,240,255,0.18);
  box-shadow: 0 0 20px rgba(0,240,255,0.04);
}

.form-label, .control-label {
  color: #D1D5DB !important;
  font-weight: 600 !important;
  font-size: 0.65rem !important;
  text-transform: uppercase !important;
  letter-spacing: 0.08em !important;
  margin-bottom: 0.2rem !important;
}

.form-select, .form-control, .shiny-input-container select {
  background: rgba(0,0,0,0.4) !important;
  border: 1px solid rgba(0,240,255,0.12) !important;
  color: #F9FAFB !important;
  border-radius: 8px !important;
  padding: 0.3rem 0.6rem !important;
  font-size: 0.75rem !important;
  font-family: "Inter", sans-serif !important;
  transition: all 0.2s ease !important;
}

.form-select:focus, .form-control:focus {
  border-color: #00F0FF !important;
  box-shadow: 0 0 0 2px rgba(0,240,255,0.12) !important;
  outline: none !important;
}

.shiny-input-container { margin-bottom: 0.4rem !important; }

.irs--shiny .irs-bar {
  background: linear-gradient(90deg, #00F0FF, #A855F7) !important;
  border: none !important; height: 3px !important; border-radius: 2px;
}

.irs--shiny .irs-line {
  background: rgba(255,255,255,0.08) !important;
  border: none !important; height: 3px !important; border-radius: 2px;
}

.irs--shiny .irs-handle {
  width: 14px !important; height: 14px !important; top: 23px !important;
  border-radius: 50% !important; background: #00F0FF !important;
  border: 2px solid #030712 !important;
  box-shadow: 0 0 8px rgba(0,240,255,0.4) !important; cursor: pointer;
}

.irs--shiny .irs-handle > i:first-child { display: none !important; }

.irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to {
  background: rgba(0,240,255,0.15) !important; color: #00F0FF !important;
  font-family: "JetBrains Mono", monospace !important;
  font-size: 0.6rem !important; font-weight: 600 !important;
  border-radius: 4px !important; padding: 1px 5px !important;
}

.irs--shiny .irs-min, .irs--shiny .irs-max {
  color: #6B7280 !important; font-family: "JetBrains Mono", monospace !important;
  font-size: 0.55rem !important; background: transparent !important;
}

.irs--shiny .irs-grid-text { color: #4B5563 !important; font-size: 0.5rem !important; }

.metric-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
  gap: 0.5rem; margin-top: 0.6rem;
}

.metric-box {
  background: rgba(17,24,39,0.65); backdrop-filter: blur(12px);
  border: 1px solid rgba(0,240,255,0.08); border-radius: 10px;
  padding: 0.6rem 0.8rem; position: relative; overflow: hidden;
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
}

.metric-box::before {
  content: ""; position: absolute; top: 0; left: 0; right: 0;
  height: 2px; background: linear-gradient(90deg, #00F0FF, #A855F7); opacity: 0.6;
}

.metric-box:hover {
  border-color: rgba(0,240,255,0.2); transform: translateY(-1px);
  box-shadow: 0 4px 16px rgba(0,240,255,0.06);
}

.metric-label {
  font-size: 0.55rem; font-weight: 600; text-transform: uppercase;
  letter-spacing: 0.1em; color: #6B7280; margin-bottom: 0.2rem;
}

.metric-value {
  font-family: "JetBrains Mono", monospace; font-size: 1.1rem; font-weight: 700;
  background: linear-gradient(135deg, #00F0FF 0%, #A855F7 100%);
  -webkit-background-clip: text; -webkit-text-fill-color: transparent;
  background-clip: text; line-height: 1.2;
}

.metric-unit { font-size: 0.55rem; color: #9CA3AF; font-weight: 400; margin-left: 0.1rem; }

.metric-value.warn-nan {
  background: linear-gradient(135deg, #F87171 0%, #FBBF24 100%) !important;
  -webkit-background-clip: text; -webkit-text-fill-color: transparent;
  background-clip: text;
}

.image-panel {
  background: rgba(17,24,39,0.5); border: 1px solid rgba(0,240,255,0.06);
  border-radius: 12px; padding: 0.5rem; text-align: center;
  min-height: 260px; display: flex; flex-direction: column;
  align-items: center; justify-content: center;
}

.image-panel img { max-width: 100%; max-height: 340px; border-radius: 8px; object-fit: contain; }

.image-label {
  font-size: 0.55rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.12em; margin-bottom: 0.4rem; padding: 0.2rem 0.6rem;
  border-radius: 6px; display: inline-block;
}

.label-original { color: #34D399; background: rgba(52,211,153,0.08); border: 1px solid rgba(52,211,153,0.15); }
.label-simulated { color: #F472B6; background: rgba(244,114,182,0.08); border: 1px solid rgba(244,114,182,0.15); }

.section-header { display: flex; align-items: center; gap: 0.4rem; margin-bottom: 0.6rem; }

.section-icon {
  width: 26px; height: 26px; border-radius: 7px; display: flex;
  align-items: center; justify-content: center; font-size: 0.8rem; flex-shrink: 0;
}

.icon-cyan { background: rgba(0,240,255,0.1); border: 1px solid rgba(0,240,255,0.15); color: #00F0FF; }
.icon-purple { background: rgba(168,85,247,0.1); border: 1px solid rgba(168,85,247,0.15); color: #A855F7; }

.section-title { font-size: 0.82rem; font-weight: 700; color: #F9FAFB; letter-spacing: -0.01em; }
.section-subtitle { font-size: 0.6rem; color: #6B7280; font-weight: 400; }

.btn-file {
  background: linear-gradient(135deg, rgba(0,240,255,0.15), rgba(168,85,247,0.15)) !important;
  border: 1px solid rgba(0,240,255,0.2) !important; color: #00F0FF !important;
  font-weight: 600 !important; font-size: 0.68rem !important;
  border-radius: 8px !important; padding: 0.25rem 0.5rem !important;
}

.sidebar-divider {
  height: 1px; background: linear-gradient(90deg, transparent, rgba(0,240,255,0.15), transparent);
  margin: 0.6rem 0; border: none;
}

.svd-info-panel {
  background: rgba(168,85,247,0.06); border: 1px solid rgba(168,85,247,0.12);
  border-radius: 10px; padding: 0.7rem 0.9rem; margin-top: 0.5rem;
}

.svd-info-panel h5 { color: #A855F7; font-weight: 700; font-size: 0.68rem; text-transform: uppercase; letter-spacing: 0.06em; margin-bottom: 0.3rem; }
.svd-info-panel p, .svd-info-panel li { color: #D1D5DB; font-size: 0.7rem; line-height: 1.4; }
.svd-info-panel code { background: rgba(0,0,0,0.3); color: #00F0FF; padding: 0.1rem 0.3rem; border-radius: 4px; font-family: "JetBrains Mono", monospace; font-size: 0.62rem; }

.btn-simulate {
  width: 100%; padding: 0.45rem 0.8rem !important; font-weight: 700 !important;
  font-size: 0.7rem !important; text-transform: uppercase !important;
  letter-spacing: 0.08em !important; border: none !important; border-radius: 8px !important;
  background: linear-gradient(135deg, #00F0FF 0%, #A855F7 100%) !important;
  color: #030712 !important; cursor: pointer;
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1) !important;
  box-shadow: 0 3px 14px rgba(0,240,255,0.2) !important;
  position: relative; overflow: hidden;
}

.btn-simulate:hover { transform: translateY(-1px); box-shadow: 0 6px 20px rgba(0,240,255,0.3) !important; }

.status-bar {
  display: flex; align-items: center; gap: 0.3rem; padding: 0.3rem 0.5rem;
  background: rgba(0,0,0,0.3); border-radius: 6px; margin-top: 0.4rem;
  font-size: 0.58rem; color: #6B7280;
}

.status-dot {
  width: 5px; height: 5px; border-radius: 50%; background: #34D399;
  box-shadow: 0 0 6px rgba(52,211,153,0.5); animation: pulse 2s ease-in-out infinite;
}

@keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.4; } }

::-webkit-scrollbar { width: 5px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb { background: rgba(0,240,255,0.15); border-radius: 3px; }

.math-block {
  background: rgba(0,0,0,0.4); border: 1px solid rgba(0,240,255,0.08);
  border-radius: 8px; padding: 0.5rem 0.8rem;
  font-family: "JetBrains Mono", monospace; font-size: 0.68rem;
  color: #D1D5DB; margin: 0.4rem 0;
}

.acc-readout {
  background: rgba(0,240,255,0.06); border: 1px solid rgba(0,240,255,0.12);
  border-radius: 8px; padding: 0.4rem 0.6rem; margin-top: 0.3rem;
  display: flex; justify-content: space-between; align-items: center;
}

.acc-readout .acc-label {
  font-size: 0.58rem; font-weight: 600; color: #9CA3AF;
  text-transform: uppercase; letter-spacing: 0.06em;
}

.acc-readout .acc-value {
  font-family: "JetBrains Mono", monospace; font-size: 0.78rem;
  font-weight: 700; color: #00F0FF;
}

@media (max-width: 768px) {
  .metric-grid { grid-template-columns: repeat(2, 1fr); }
  .image-panel img { max-height: 200px; }
}
'

# Load the default parrots image from imager, or generate a gradient fallback
get_default_image <- function() {
  parrots <- system.file("extdata/parrots.png", package = "imager")
  if (nzchar(parrots) && file.exists(parrots)) return(parrots)
  tmp <- tempfile(fileext = ".png")
  arr <- array(0, dim = c(200, 200, 1, 3))
  for (i in 1:200) for (j in 1:200) {
    arr[i, j, 1, 1] <- i / 200
    arr[i, j, 1, 2] <- j / 200
    arr[i, j, 1, 3] <- 1 - (i + j) / 400
  }
  save.image(as.cimg(arr), tmp)
  tmp
}

# Format a metric value for display (handles NaN, Inf, etc.)
fmt_metric <- function(val, unit = "", digits = 2) {
  if (is.nan(val)) return(list(value = "N/A", unit = "(Abs. Hyperopia)", is_nan = TRUE))
  if (is.infinite(val)) return(list(value = "\u221E", unit = unit, is_nan = FALSE))
  list(value = formatC(round(val, digits), format = "f", digits = digits), unit = unit, is_nan = FALSE)
}

# UI ----

ui <- page_navbar(
  title = span(bs_icon("eye-fill"), " VisionSimR"),
  id = "main_nav",
  theme = bs_theme(
    version = 5, bootswatch = "darkly",
    bg = "#030712", fg = "#F9FAFB",
    primary = "#00F0FF", secondary = "#A855F7",
    success = "#34D399", info = "#00F0FF",
    warning = "#FBBF24", danger = "#F87171",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    heading_font = font_google("Inter"),
    "navbar-bg" = "rgba(3,7,18,0.85)",
    "border-color" = "rgba(0,240,255,0.08)"
  ),

  header = tags$head(
    tags$style(HTML(custom_css)),
    tags$meta(name = "description", content = "VisionSimR - Physics-based visual impairment simulator"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        setTimeout(function() { $('#run_sim').click(); }, 800);
      });
    "))
  ),

  # Tab 1: Simulation Lab
  nav_panel(
    title = span(bs_icon("cpu"), " Simulation Lab"),
    value = "sim_lab",
    layout_sidebar(
      sidebar = sidebar(
        width = 280, id = "sim_sidebar",
        div(class = "section-header",
          div(class = "section-icon icon-cyan", bs_icon("sliders")),
          div(div(class = "section-title", "Control Panel"),
              div(class = "section-subtitle", "Configure simulation parameters"))
        ),
        fileInput("img_upload", "Upload Image",
          accept = c("image/png", "image/jpeg", "image/bmp", "image/tiff"),
          placeholder = "Using default image..."
        ),
        tags$hr(class = "sidebar-divider"),

        selectInput("condition", "Refractive Condition", choices = c(
          "Myopia (Near-sighted)" = "myopia", "Hyperopia (Far-sighted)" = "hyperopia",
          "Presbyopia (Age-related)" = "presbyopia", "Astigmatism" = "astigmatism",
          "None (Emmetropic)" = "none"
        ), selected = "myopia"),
        selectInput("cvd_type", "Colour Vision Deficiency", choices = c(
          "None" = "none", "Protanopia" = "protanopia",
          "Deuteranopia" = "deuteranopia", "Tritanopia" = "tritanopia"
        ), selected = "none"),
        tags$hr(class = "sidebar-divider"),

        sliderInput("age", "Patient Age", min = 10, max = 80, value = 25, step = 1, post = " yrs"),
        uiOutput("acc_readout"),
        sliderInput("rx", "Prescription (Rx)", min = -10, max = 10, value = -2.5, step = 0.25, post = " D"),

        # Astigmatism-specific controls
        conditionalPanel(
          condition = "input.condition == 'astigmatism'",
          sliderInput("cylinder", "Cylinder (Cyl)", min = -6, max = 0, value = -1.5, step = 0.25, post = " D"),
          sliderInput("axis", "Axis", min = 0, max = 180, value = 90, step = 5, post = "\u00B0")
        ),

        sliderInput("distance", "Viewing Distance", min = 0.1, max = 20, value = 3, step = 0.1, post = " m"),
        sliderInput("pupil", "Pupil Diameter", min = 2, max = 8, value = 4, step = 0.5, post = " mm"),
        tags$hr(class = "sidebar-divider"),
        actionButton("run_sim", "Run Simulation", class = "btn-simulate", icon = icon("bolt")),
        div(class = "status-bar", div(class = "status-dot"), span("VisionSimR engine ready"))
      ),

      # Main content area
      div(
        div(class = "section-header",
          div(class = "section-icon icon-cyan", bs_icon("binoculars-fill")),
          div(div(class = "section-title", "Visual Comparison"),
              div(class = "section-subtitle", "Original vs. simulated perception"))
        ),
        layout_column_wrap(
          width = 1/2, heights_equal = "row", gap = "0.6rem",
          div(class = "image-panel",
            span(class = "image-label label-original", bs_icon("check-circle-fill"), " Original"),
            withSpinner(imageOutput("img_original", height = "auto"), type = 8, color = "#34D399", size = 0.6)
          ),
          div(class = "image-panel",
            span(class = "image-label label-simulated", bs_icon("eye-slash-fill"), " Simulated"),
            withSpinner(imageOutput("img_simulated", height = "auto"), type = 8, color = "#F472B6", size = 0.6)
          )
        ),
        div(style = "margin-top: 0.8rem;",
          div(class = "section-header",
            div(class = "section-icon icon-purple", bs_icon("graph-up-arrow")),
            div(div(class = "section-title", "Optical Physics"),
                div(class = "section-subtitle", "Diagnostics from thin-lens eye model + Hofstetter accommodation"))
          ),
          uiOutput("metrics_row")
        )
      )
    )
  ),

  # Tab 2: Mathematical Diagnostics
  nav_panel(
    title = span(bs_icon("calculator"), " Mathematical Diagnostics"),
    value = "math_tab",
    layout_sidebar(
      sidebar = sidebar(
        width = 280, id = "svd_sidebar",
        div(class = "section-header",
          div(class = "section-icon icon-purple", bs_icon("grid-3x3-gap-fill")),
          div(div(class = "section-title", "SVD Controls"),
              div(class = "section-subtitle", "Explore rank truncation"))
        ),
        sliderInput("svd_rank", "SVD Rank (k)", min = 1, max = 150, value = 30, step = 1),
        tags$hr(class = "sidebar-divider"),
        div(class = "svd-info-panel",
          h5(bs_icon("mortarboard-fill"), " Eckart-Young Theorem"),
          p("The best rank-", tags$em("k"), " approximation of any matrix ",
            tags$strong("M"), " in the Frobenius norm is given by its truncated SVD:"),
          div(class = "math-block",
            HTML("M<sub>k</sub> = U<sub>k</sub> &middot; &Sigma;<sub>k</sub> &middot; V<sub>k</sub><sup>T</sup>")),
          p("Where:"),
          tags$ul(
            tags$li(tags$code("U"), " - left singular vectors (spatial patterns)"),
            tags$li(tags$code("\u03A3"), " - diagonal matrix of singular values"),
            tags$li(tags$code("V"), " - right singular vectors (frequency modes)")
          ),
          p("Lower rank = fewer singular values = less detail = more blur.")
        ),
        tags$hr(class = "sidebar-divider"),
        div(class = "status-bar", div(class = "status-dot"), span("Adjust rank to see image degradation"))
      ),

      # SVD main panel
      div(
        div(class = "section-header",
          div(class = "section-icon icon-purple", bs_icon("cpu-fill")),
          div(div(class = "section-title", "SVD Rank Reconstruction"),
              div(class = "section-subtitle", "See how information loss maps to visual blur"))
        ),
        layout_column_wrap(
          width = 1/2, heights_equal = "row", gap = "0.6rem",
          div(class = "image-panel",
            span(class = "image-label label-original", bs_icon("image-fill"), " Original"),
            withSpinner(imageOutput("svd_original", height = "auto"), type = 8, color = "#34D399", size = 0.6)
          ),
          div(class = "image-panel",
            span(class = "image-label label-simulated", bs_icon("grid-3x3"), " Rank-k Reconstruction"),
            withSpinner(imageOutput("svd_reconstructed", height = "auto"), type = 8, color = "#A855F7", size = 0.6)
          )
        ),
        div(style = "margin-top: 0.8rem;",
          div(class = "section-header",
            div(class = "section-icon icon-cyan", bs_icon("bar-chart-fill")),
            div(div(class = "section-title", "Reconstruction Analysis"),
                div(class = "section-subtitle", "Quantitative SVD rank metrics"))
          ),
          uiOutput("svd_metrics")
        ),
        div(style = "margin-top: 0.8rem;",
          div(class = "section-header",
            div(class = "section-icon icon-cyan", bs_icon("activity")),
            div(div(class = "section-title", "Singular Value Spectrum"),
                div(class = "section-subtitle", "Energy distribution across rank components"))
          ),
          div(class = "glass-card",
            withSpinner(plotOutput("sv_spectrum", height = "260px"), type = 8, color = "#00F0FF", size = 0.6)
          )
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(tags$span(style = "color:#4B5563; font-size:0.62rem; font-weight:500;", "v0.2.0 \u00B7 Hofstetter + Conoid of Sturm Engine"))
)

# Server ----

server <- function(input, output, session) {

  # Track which image to use
  current_image_path <- reactiveVal(NULL)

  observe({
    uploaded <- input$img_upload
    if (!is.null(uploaded)) current_image_path(uploaded$datapath)
  })

  observe({
    if (is.null(current_image_path())) current_image_path(get_default_image())
  }, priority = 100)

  # Load image and resize for performance
  load_and_prep <- function(img_path, max_dim = 400L) {
    img <- load.image(img_path)
    if (spectrum(img) == 4L) img <- rm.alpha(img)
    if (spectrum(img) == 1L) img <- add.color(img)
    W <- width(img); H <- height(img)
    s <- min(1.0, max_dim / max(W, H))
    if (s < 1.0) img <- resize(img, round(W * s), round(H * s), interpolation_type = 5L)
    img
  }

  # Hofstetter accommodation (duplicated here since it's not exported from the package)
  calc_hofstetter <- function(age) max(0, 18.5 - 0.3 * age)

  calc_near_point <- function(Rx_D, max_acc) {
    V_RET <- 0.017
    total_power <- ((1 / V_RET) - Rx_D) + max_acc
    vergence <- total_power - (1 / V_RET)
    if (vergence <= 0) return(Inf)
    1 / vergence
  }

  # Accommodation readout
  output$acc_readout <- renderUI({
    acc <- calc_hofstetter(input$age)
    div(class = "acc-readout",
      span(class = "acc-label", "Acc (Hofstetter)"),
      span(class = "acc-value", sprintf("%.1f D", acc))
    )
  })

  # Run simulation when button clicked
  sim_results <- eventReactive(input$run_sim, {
    img_path <- current_image_path()
    req(img_path)

    condition <- input$condition
    cvd <- input$cvd_type
    rx <- input$rx
    age <- input$age
    dist <- input$distance
    pupil <- input$pupil
    cyl <- if (!is.null(input$cylinder)) input$cylinder else 0
    ax <- if (!is.null(input$axis)) input$axis else 180
    acc <- calc_hofstetter(age)

    tryCatch({
      # No condition and no CVD = just show the original
      if (condition == "none" && cvd == "none") {
        img <- load_and_prep(img_path)
        return(list(original = img, simulated = img, defocus_D = 0,
                    blur_px = 0, far_point_m = Inf,
                    near_point_m = calc_near_point(0, acc),
                    age = age, accommodation_D = acc,
                    cylinder_D = 0, axis_deg = 180))
      }

      cvd_arg <- if (cvd == "none") NULL else cvd

      # CVD only (no refractive condition)
      if (condition == "none") {
        img <- load_and_prep(img_path)
        sim_arr <- apply_cvd(as.array(img), cvd)
        return(list(original = img, simulated = as.cimg(sim_arr), defocus_D = 0,
                    blur_px = 0, far_point_m = Inf,
                    near_point_m = calc_near_point(0, acc),
                    age = age, accommodation_D = acc,
                    cylinder_D = 0, axis_deg = 180))
      }

      # Full simulation
      simulate_vision(
        image_path = img_path, condition = condition, Rx_D = rx,
        age = age, cylinder_D = cyl, axis_deg = ax,
        distance_m = dist, pupil_mm = pupil, color_deficiency = cvd_arg,
        show_plot = FALSE
      )
    }, error = function(e) {
      showNotification(paste("Simulation error:", e$message), type = "error", duration = 8)
      NULL
    })
  }, ignoreNULL = FALSE)

  # Render original and simulated images
  output$img_original <- renderImage({
    res <- sim_results(); req(res)
    tmp <- tempfile(fileext = ".png")
    save.image(res$original, tmp)
    list(src = tmp, contentType = "image/png", width = "100%", alt = "Original image")
  }, deleteFile = TRUE)

  output$img_simulated <- renderImage({
    res <- sim_results(); req(res)
    tmp <- tempfile(fileext = ".png")
    save.image(res$simulated, tmp)
    list(src = tmp, contentType = "image/png", width = "100%", alt = "Simulated")
  }, deleteFile = TRUE)

  # Metrics panel
  output$metrics_row <- renderUI({
    res <- sim_results(); req(res)
    defocus <- fmt_metric(abs(res$defocus_D), "D")
    blur <- fmt_metric(res$blur_px, "px")
    fp <- fmt_metric(res$far_point_m, "m")
    np <- fmt_metric(res$near_point_m, "m")
    acc_val <- if (!is.null(res$accommodation_D)) res$accommodation_D else calc_hofstetter(input$age)

    fp_class <- if (isTRUE(fp$is_nan)) "metric-value warn-nan" else "metric-value"

    div(class = "metric-grid",
      div(class = "metric-box", div(class = "metric-label", "Defocus"),
        div(class = "metric-value", defocus$value, span(class = "metric-unit", defocus$unit))),
      div(class = "metric-box", div(class = "metric-label", "Blur Radius"),
        div(class = "metric-value", blur$value, span(class = "metric-unit", blur$unit))),
      div(class = "metric-box", div(class = "metric-label", "Accommodation"),
        div(class = "metric-value", formatC(acc_val, format = "f", digits = 1), span(class = "metric-unit", "D"))),
      div(class = "metric-box", div(class = "metric-label", "Far Point"),
        div(class = fp_class, fp$value, span(class = "metric-unit", fp$unit))),
      div(class = "metric-box", div(class = "metric-label", "Near Point"),
        div(class = "metric-value", np$value, span(class = "metric-unit", np$unit)))
    )
  })

  # SVD Diagnostics tab ----

  svd_image <- reactive({
    img_path <- current_image_path(); req(img_path)
    load_and_prep(img_path, max_dim = 300L)
  })

  svd_data <- reactive({
    img <- svd_image(); req(img)
    W <- width(img); H <- height(img)
    arr <- as.array(img)
    list(
      svd_r = svd(matrix(arr[,,1,1], W, H)),
      svd_g = svd(matrix(arr[,,1,2], W, H)),
      svd_b = svd(matrix(arr[,,1,3], W, H)),
      W = W, H = H, max_rank = min(W, H)
    )
  })

  observe({
    data <- svd_data(); req(data)
    updateSliderInput(session, "svd_rank", max = data$max_rank)
  })

  output$svd_original <- renderImage({
    img <- svd_image(); req(img)
    tmp <- tempfile(fileext = ".png"); save.image(img, tmp)
    list(src = tmp, contentType = "image/png", width = "100%", alt = "Original")
  }, deleteFile = TRUE)

  output$svd_reconstructed <- renderImage({
    data <- svd_data(); req(data)
    k <- min(input$svd_rank, data$max_rank)

    recon <- function(sv, k) {
      u <- sv$u[, seq_len(k), drop = FALSE]
      d <- diag(sv$d[seq_len(k)], nrow = k)
      v <- sv$v[, seq_len(k), drop = FALSE]
      matrix(pmax(0, pmin(1, as.numeric(u %*% d %*% t(v)))), data$W, data$H)
    }

    out <- array(0, dim = c(data$W, data$H, 1L, 3L))
    out[,,1,1] <- recon(data$svd_r, k)
    out[,,1,2] <- recon(data$svd_g, k)
    out[,,1,3] <- recon(data$svd_b, k)
    tmp <- tempfile(fileext = ".png"); save.image(as.cimg(out), tmp)
    list(src = tmp, contentType = "image/png", width = "100%", alt = paste0("Rank-", k))
  }, deleteFile = TRUE)

  output$svd_metrics <- renderUI({
    data <- svd_data(); req(data)
    k <- min(input$svd_rank, data$max_rank)
    epct <- function(sv, k) sum(sv$d[seq_len(k)]^2) / sum(sv$d^2) * 100
    avg_e <- mean(c(epct(data$svd_r, k), epct(data$svd_g, k), epct(data$svd_b, k)))
    pct_r <- round(k / data$max_rank * 100, 1)
    comp <- round((data$W * data$H) / (k * (data$W + data$H + 1)), 1)
    div(class = "metric-grid",
      div(class = "metric-box", div(class = "metric-label", "SVD Rank (k)"),
        div(class = "metric-value", k, span(class = "metric-unit", paste0("/ ", data$max_rank)))),
      div(class = "metric-box", div(class = "metric-label", "Energy Retained"),
        div(class = "metric-value", formatC(avg_e, format = "f", digits = 1), span(class = "metric-unit", "%"))),
      div(class = "metric-box", div(class = "metric-label", "Rank Utilization"),
        div(class = "metric-value", formatC(pct_r, format = "f", digits = 1), span(class = "metric-unit", "%"))),
      div(class = "metric-box", div(class = "metric-label", "Compression Ratio"),
        div(class = "metric-value", paste0(comp, "\u00D7")))
    )
  })

  output$sv_spectrum <- renderPlot({
    data <- svd_data(); req(data)
    k <- min(input$svd_rank, data$max_rank)
    max_len <- max(length(data$svd_r$d), length(data$svd_g$d), length(data$svd_b$d))
    avg_sv <- (data$svd_r$d[seq_len(max_len)] + data$svd_g$d[seq_len(max_len)] + data$svd_b$d[seq_len(max_len)]) / 3

    par(bg = "transparent", fg = "#9CA3AF", col.axis = "#6B7280", col.lab = "#9CA3AF",
        col.main = "#F9FAFB", family = "sans", mar = c(3.5, 4, 1.5, 0.5))
    plot(seq_along(avg_sv), avg_sv, type = "n", xlab = "Singular Value Index",
         ylab = "Magnitude", main = "", las = 1, cex.axis = 0.7, cex.lab = 0.75)
    grid(col = adjustcolor("#FFFFFF", alpha.f = 0.05), lty = 1)
    if (k > 0) {
      polygon(c(1, seq_len(k), k), c(0, avg_sv[seq_len(k)], 0),
              col = adjustcolor("#00F0FF", alpha.f = 0.08), border = NA)
    }
    lines(seq_along(avg_sv), avg_sv, col = "#4B5563", lwd = 1.2)
    if (k > 0) {
      lines(seq_len(k), avg_sv[seq_len(k)], col = "#00F0FF", lwd = 2)
      points(seq_len(k), avg_sv[seq_len(k)], col = "#00F0FF", pch = 16, cex = 0.3)
    }
    abline(v = k, col = "#A855F7", lwd = 1.5, lty = 2)
    legend("topright",
      legend = c(paste0("Retained (k=", k, ")"), "Discarded", "Cutoff"),
      col = c("#00F0FF", "#4B5563", "#A855F7"), lwd = c(2, 1.2, 1.5), lty = c(1, 1, 2),
      bg = adjustcolor("#111827", alpha.f = 0.9), text.col = "#D1D5DB", cex = 0.65,
      box.col = adjustcolor("#00F0FF", alpha.f = 0.1))
  }, bg = "transparent")
}

shinyApp(ui = ui, server = server)

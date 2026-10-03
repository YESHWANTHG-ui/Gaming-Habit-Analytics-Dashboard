# ============================================================
# 🎮 GAMING HABIT ANALYTICS DASHBOARD
# MODERN GAMING WEBSITE
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)

# ============================================================
# 1. FOLDERS
# ============================================================

if (!dir.exists("data")) {
  dir.create("data")
}

if (!dir.exists("www")) {
  dir.create("www")
}

csv_file <- "data/gaming_data.csv"


# ============================================================
# 2. SAMPLE DATA
# ============================================================

sample_data <- data.frame(
  
  Player_ID = 1:10,
  
  Player_Name = c(
    "Arun",
    "Rahul",
    "Kiran",
    "Mikey",
    "Vijay",
    "Yeshwanth",
    "Ajay",
    "Surya",
    "Rohan",
    "Dinesh"
  ),
  
  Age = c(
    20, 21, 19, 22, 20,
    21, 23, 19, 22, 20
  ),
  
  Game = c(
    "Valorant",
    "PUBG",
    "Minecraft",
    "GTA V",
    "Free Fire",
    "Valorant",
    "FIFA",
    "Minecraft",
    "Call of Duty",
    "Fortnite"
  ),
  
  Platform = c(
    "PC",
    "Mobile",
    "PC",
    "PC",
    "Mobile",
    "PC",
    "Console",
    "PC",
    "Mobile",
    "PC"
  ),
  
  Genre = c(
    "FPS",
    "Battle Royale",
    "Sandbox",
    "Action",
    "Battle Royale",
    "FPS",
    "Sports",
    "Sandbox",
    "FPS",
    "Battle Royale"
  ),
  
  Hours_Per_Day = c(
    4.5,
    5.2,
    3.5,
    6.0,
    4.0,
    5.5,
    2.5,
    4.2,
    6.5,
    5.0
  ),
  
  Days_Per_Week = c(
    6, 7, 5, 6, 7,
    6, 4, 5, 7, 6
  ),
  
  Session_Minutes = c(
    90, 120, 75, 150, 100,
    110, 60, 90, 160, 130
  ),
  
  Sleep_Hours = c(
    7.0, 6.2, 8.0, 5.5, 6.5,
    6.8, 8.0, 7.5, 5.8, 6.2
  ),
  
  Screen_Time = c(
    7.5, 8.0, 6.0, 9.0, 7.0,
    8.2, 5.5, 6.8, 9.5, 8.0
  ),
  
  Gaming_Spending = c(
    1200, 800, 500, 2500, 700,
    1500, 900, 600, 2000, 1100
  ),
  
  Mood = c(
    "Happy",
    "Neutral",
    "Happy",
    "Stressed",
    "Happy",
    "Happy",
    "Relaxed",
    "Happy",
    "Stressed",
    "Neutral"
  ),
  
  stringsAsFactors = FALSE
)


# ============================================================
# 3. CREATE CSV
# ============================================================

if (!file.exists(csv_file)) {
  
  write.csv(
    sample_data,
    csv_file,
    row.names = FALSE
  )
  
}


# ============================================================
# 4. LOAD CSV
# ============================================================

gaming_data <- read.csv(
  csv_file,
  stringsAsFactors = FALSE
)


# ============================================================
# 5. IMAGE FUNCTION
# ============================================================

game_image <- function(
    file,
    height = "190px"
) {
  
  path <- file.path(
    "www",
    file
  )
  
  if (file.exists(path)) {
    
    tags$img(
      src = file,
      style = paste0(
        "width:100%;",
        "height:", height, ";",
        "object-fit:cover;",
        "display:block;"
      )
    )
    
  } else {
    
    div(
      
      style = paste0(
        "height:", height, ";",
        "display:flex;",
        "align-items:center;",
        "justify-content:center;",
        "background:#111827;",
        "color:#64748b;",
        "font-size:16px;"
      ),
      
      paste(
        "Image not found:",
        file
      )
      
    )
    
  }
  
}


# ============================================================
# 6. CSS
# ============================================================

custom_css <- tags$head(
  
  tags$style(
    
    HTML("

/* ============================================================
   GLOBAL
   ============================================================ */

body {
  font-family: 'Segoe UI', sans-serif;
  background: #050816 !important;
  color: white !important;
}

.content-wrapper,
.right-side {
  background:
    radial-gradient(
      circle at top right,
      rgba(99,102,241,0.10),
      transparent 30%
    ),
    #050816 !important;
}


/* ============================================================
   SIDEBAR
   ============================================================ */

.main-sidebar {
  background:
    linear-gradient(
      180deg,
      #070b18,
      #0b1020
    ) !important;
}

.main-header .logo {
  background:
    linear-gradient(
      135deg,
      #2563eb,
      #7c3aed
    ) !important;

  font-weight: 800;
  font-size: 20px !important;
}

.main-header .navbar {
  background: #080d1b !important;
}

.sidebar-menu > li > a {
  color: #94a3b8 !important;
  padding: 16px 20px;
  transition: 0.3s;
}

.sidebar-menu > li > a:hover {
  color: white !important;

  background:
    linear-gradient(
      90deg,
      rgba(37,99,235,0.35),
      transparent
    ) !important;
}

.sidebar-menu > .active > a {
  color: white !important;

  background:
    linear-gradient(
      90deg,
      #2563eb,
      #7c3aed
    ) !important;
}


/* ============================================================
   HERO
   ============================================================ */

.hero {
  position: relative;
  height: 430px;
  overflow: hidden;
  border-radius: 28px;
  margin-bottom: 30px;

  box-shadow:
    0 25px 70px rgba(0,0,0,0.65);
}

.hero img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  opacity: 0.55;
}

.hero-overlay {
  position: absolute;

  top: 0;
  left: 0;

  width: 100%;
  height: 100%;

  padding: 55px;

  display: flex;
  flex-direction: column;
  justify-content: center;

  background:
    linear-gradient(
      90deg,
      rgba(4,7,18,0.97),
      rgba(4,7,18,0.65),
      rgba(4,7,18,0.10)
    );
}

.hero-small {
  color: #60a5fa;
  font-size: 14px;
  font-weight: 700;
  letter-spacing: 3px;
  text-transform: uppercase;
  margin-bottom: 15px;
}

.hero-title {
  font-size: 52px;
  font-weight: 900;
  line-height: 1.05;
  max-width: 700px;

  background:
    linear-gradient(
      90deg,
      #60a5fa,
      #a78bfa,
      #f472b6
    );

  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
}

.hero-description {
  color: #cbd5e1;
  font-size: 17px;
  max-width: 620px;
  line-height: 1.7;
  margin-top: 18px;
}

.hero-button {
  display: inline-block;
  margin-top: 25px;
  padding: 13px 25px;
  width: max-content;

  border-radius: 12px;
  color: white !important;
  font-weight: 700;

  background:
    linear-gradient(
      135deg,
      #2563eb,
      #7c3aed
    );

  box-shadow:
    0 10px 30px rgba(37,99,235,0.30);
}


/* ============================================================
   SECTION TITLE
   ============================================================ */

.section-title {
  color: white;
  font-size: 28px;
  font-weight: 800;
  margin-top: 35px;
  margin-bottom: 20px;
}

.section-title span {
  color: #60a5fa;
}


/* ============================================================
   GAME CARDS
   ============================================================ */

.game-card {
  position: relative;
  overflow: hidden;

  border-radius: 20px;

  background: #0d1324;

  border:
    1px solid
    rgba(255,255,255,0.06);

  margin-bottom: 25px;

  transition:
    transform 0.35s ease,
    box-shadow 0.35s ease,
    border 0.35s ease;

  box-shadow:
    0 15px 40px rgba(0,0,0,0.35);
}

.game-card:hover {
  transform: translateY(-10px);

  border-color:
    rgba(96,165,250,0.60);

  box-shadow:
    0 25px 60px
    rgba(37,99,235,0.25);
}

.game-image {
  position: relative;
  height: 210px;
  overflow: hidden;
}

.game-image img {
  width: 100%;
  height: 100%;
  object-fit: cover;

  transition:
    transform 0.5s ease;
}

.game-card:hover
.game-image img {
  transform: scale(1.10);
}

.game-overlay {
  position: absolute;

  left: 0;
  bottom: 0;

  width: 100%;

  padding:
    45px 18px 15px;

  background:
    linear-gradient(
      transparent,
      rgba(0,0,0,0.9)
    );
}

.game-badge {
  display: inline-block;

  padding: 5px 9px;

  border-radius: 7px;

  background:
    rgba(37,99,235,0.85);

  color: white;

  font-size: 10px;
  font-weight: 700;

  letter-spacing: 1px;
  text-transform: uppercase;
}

.game-card-body {
  padding: 18px;
}

.game-title {
  color: white;

  font-size: 21px;
  font-weight: 800;

  margin-bottom: 7px;
}

.game-info {
  color: #64748b;
  font-size: 13px;
}

.game-stat {
  display: flex;
  justify-content: space-between;

  margin-top: 15px;
  padding-top: 12px;

  border-top:
    1px solid
    rgba(255,255,255,0.06);

  color: #94a3b8;
  font-size: 12px;
}


/* ============================================================
   KPI
   ============================================================ */

.kpi {
  background:
    linear-gradient(
      145deg,
      #10182a,
      #0b1020
    );

  border-radius: 18px;

  padding: 20px;

  min-height: 145px;

  border:
    1px solid
    rgba(255,255,255,0.06);

  transition: 0.3s;
}

.kpi:hover {
  transform: translateY(-5px);

  border-color:
    rgba(96,165,250,0.45);
}

.kpi-icon {
  font-size: 28px;
  margin-bottom: 10px;
}

.kpi-label {
  color: #64748b;
  font-size: 12px;

  text-transform: uppercase;
  letter-spacing: 1px;
}

.kpi-value {
  color: white;

  font-size: 27px;
  font-weight: 800;

  margin-top: 5px;
}


/* ============================================================
   CHART BOX
   ============================================================ */

.chart-card {
  background: #0d1324;

  border-radius: 20px;

  padding: 20px;

  border:
    1px solid
    rgba(255,255,255,0.06);

  margin-bottom: 25px;

  box-shadow:
    0 15px 35px rgba(0,0,0,0.25);
}

.chart-title {
  color: white;

  font-size: 18px;
  font-weight: 700;

  margin-bottom: 15px;
}


/* ============================================================
   FILTERS
   ============================================================ */

.filter-card {
  background:
    linear-gradient(
      145deg,
      #111827,
      #0b1020
    );

  padding: 23px;

  border-radius: 20px;

  border:
    1px solid
    rgba(255,255,255,0.06);

  margin-bottom: 30px;
}

.filter-heading {
  color: white;

  font-size: 18px;
  font-weight: 700;

  margin-bottom: 15px;
}


/* ============================================================
   FORM
   ============================================================ */

.form-control,
.selectize-input {
  background: #111827 !important;
  color: white !important;

  border:
    1px solid
    #263247 !important;

  border-radius: 10px !important;
}

.selectize-dropdown {
  background: #111827 !important;
  color: white !important;
}

label {
  color: #cbd5e1 !important;
}


/* ============================================================
   BUTTONS
   ============================================================ */

.btn-primary {
  background:
    linear-gradient(
      135deg,
      #2563eb,
      #7c3aed
    ) !important;

  border: none !important;

  border-radius: 10px !important;

  padding: 11px 20px;

  font-weight: 700;
}


/* ============================================================
   DATA TABLE
   ============================================================ */

.dataTables_wrapper {
  background: #0d1324 !important;
  color: white !important;

  padding: 15px;

  border-radius: 18px;
}

table.dataTable {
  background: #0d1324 !important;
  color: #cbd5e1 !important;
}

table.dataTable thead th {
  background: #111827 !important;
  color: #60a5fa !important;
}


/* ============================================================
   PAGE HEADER
   ============================================================ */

.page-title {
  color: white;

  font-size: 34px;
  font-weight: 900;

  margin-bottom: 8px;
}

.page-description {
  color: #64748b;
  margin-bottom: 25px;
}


/* ============================================================
   IMAGE BANNER
   ============================================================ */

.image-banner {
  overflow: hidden;

  border-radius: 22px;

  margin-bottom: 25px;

  box-shadow:
    0 20px 50px rgba(0,0,0,0.35);
}


/* ============================================================
   INFO CARD
   ============================================================ */

.info-card {
  background: #0d1324;

  border-radius: 20px;

  padding: 25px;

  border:
    1px solid
    rgba(255,255,255,0.06);

  margin-bottom: 25px;
}

")
  )
)


# ============================================================
# 7. UI
# ============================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "🎮 GAMING HUB"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Home",
        tabName = "dashboard",
        icon = icon("home")
      ),
      
      menuItem(
        "Add Gaming Record",
        tabName = "add",
        icon = icon("plus-circle")
      ),
      
      menuItem(
        "Manage Records",
        tabName = "manage",
        icon = icon("database")
      ),
      
      menuItem(
        "Analytics",
        tabName = "analytics",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Gaming Data",
        tabName = "data",
        icon = icon("table")
      )
      
    )
  ),
  
  dashboardBody(
    
    custom_css,
    
    tabItems(
      
      # ========================================================
      # HOME
      # ========================================================
      
      tabItem(
        
        tabName = "dashboard",
        
        # HERO
        div(
          
          class = "hero",
          
          game_image(
            "dashboard.jpg",
            height = "430px"
          ),
          
          div(
            
            class = "hero-overlay",
            
            div(
              class = "hero-small",
              "WELCOME TO THE GAMING HUB"
            ),
            
            div(
              class = "hero-title",
              "Understand Your Gaming Life"
            ),
            
            div(
              class = "hero-description",
              
              "Track your gaming hours, favorite games,
              sleep, screen time, mood and spending
              through one powerful interactive dashboard."
              
            ),
            
            div(
              class = "hero-button",
              "🎮 EXPLORE DASHBOARD"
            )
            
          )
          
        ),
        
        
        # FILTER
        div(
          
          class = "filter-card",
          
          div(
            class = "filter-heading",
            "🎯 Explore Gaming Data"
          ),
          
          fluidRow(
            
            column(
              3,
              
              selectInput(
                "filter_game",
                "Game",
                
                choices = c(
                  "All",
                  sort(unique(gaming_data$Game))
                )
              )
              
            ),
            
            column(
              3,
              
              selectInput(
                "filter_genre",
                "Genre",
                
                choices = c(
                  "All",
                  sort(unique(gaming_data$Genre))
                )
              )
              
            ),
            
            column(
              3,
              
              selectInput(
                "filter_platform",
                "Platform",
                
                choices = c(
                  "All",
                  sort(unique(gaming_data$Platform))
                )
              )
              
            ),
            
            column(
              3,
              
              selectInput(
                "filter_mood",
                "Mood",
                
                choices = c(
                  "All",
                  sort(unique(gaming_data$Mood))
                )
              )
              
            )
            
          ),
          
          fluidRow(
            
            column(
              
              4,
              
              selectInput(
                
                "filter_age",
                
                "Age Group",
                
                choices = c(
                  "All",
                  "18-20",
                  "21-23",
                  "24+"
                )
                
              )
              
            ),
            
            column(
              
              4,
              
              br(),
              
              actionButton(
                "reset_filters",
                "↻ Reset Filters",
                class = "btn-primary"
              )
              
            )
            
          )
          
        ),
        
        
        # KPI
        div(
          class = "section-title",
          "📊 Gaming Overview"
        ),
        
        fluidRow(
          
          column(
            2,
            
            div(
              
              class = "kpi",
              
              div(
                class = "kpi-icon",
                "👥"
              ),
              
              div(
                class = "kpi-label",
                "Players"
              ),
              
              div(
                class = "kpi-value",
                textOutput("total_players")
              )
              
            )
          ),
          
          column(
            2,
            
            div(
              
              class = "kpi",
              
              div(
                class = "kpi-icon",
                "🎮"
              ),
              
              div(
                class = "kpi-label",
                "Avg Gaming"
              ),
              
              div(
                class = "kpi-value",
                textOutput("avg_hours")
              )
              
            )
          ),
          
          column(
            2,
            
            div(
              
              class = "kpi",
              
              div(
                class = "kpi-icon",
                "😴"
              ),
              
              div(
                class = "kpi-label",
                "Avg Sleep"
              ),
              
              div(
                class = "kpi-value",
                textOutput("avg_sleep")
              )
              
            )
          ),
          
          column(
            2,
            
            div(
              
              class = "kpi",
              
              div(
                class = "kpi-icon",
                "💰"
              ),
              
              div(
                class = "kpi-label",
                "Spending"
              ),
              
              div(
                class = "kpi-value",
                textOutput("total_spending")
              )
              
            )
          ),
          
          column(
            2,
            
            div(
              
              class = "kpi",
              
              div(
                class = "kpi-icon",
                "🔥"
              ),
              
              div(
                class = "kpi-label",
                "High Gamers"
              ),
              
              div(
                class = "kpi-value",
                textOutput("high_players")
              )
              
            )
          )
          
        ),
        
        
        # ========================================================
        # FEATURED GAMES
        # ========================================================
        
        div(
          class = "section-title",
          "🔥 Featured Games"
        ),
        
        fluidRow(
          
          # VALORANT
          column(
            
            3,
            
            div(
              
              class = "game-card",
              
              div(
                
                class = "game-image",
                
                game_image(
                  "valorant.jpg",
                  height = "210px"
                ),
                
                div(
                  
                  class = "game-overlay",
                  
                  div(
                    class = "game-badge",
                    "FPS"
                  )
                  
                )
                
              ),
              
              div(
                
                class = "game-card-body",
                
                div(
                  class = "game-title",
                  "VALORANT"
                ),
                
                div(
                  class = "game-info",
                  "Competitive Tactical Shooter"
                ),
                
                div(
                  
                  class = "game-stat",
                  
                  span("Platform"),
                  span("PC")
                  
                )
                
              )
              
            )
            
          ),
          
          
          # MINECRAFT
          column(
            
            3,
            
            div(
              
              class = "game-card",
              
              div(
                
                class = "game-image",
                
                game_image(
                  "minecraft.jpg",
                  height = "210px"
                ),
                
                div(
                  
                  class = "game-overlay",
                  
                  div(
                    class = "game-badge",
                    "SANDBOX"
                  )
                  
                )
                
              ),
              
              div(
                
                class = "game-card-body",
                
                div(
                  class = "game-title",
                  "MINECRAFT"
                ),
                
                div(
                  class = "game-info",
                  "Creative Open World"
                ),
                
                div(
                  
                  class = "game-stat",
                  
                  span("Platform"),
                  span("PC")
                  
                )
                
              )
              
            )
            
          ),
          
          
          # GTA
          column(
            
            3,
            
            div(
              
              class = "game-card",
              
              div(
                
                class = "game-image",
                
                game_image(
                  "gta.jpg",
                  height = "210px"
                ),
                
                div(
                  
                  class = "game-overlay",
                  
                  div(
                    class = "game-badge",
                    "ACTION"
                  )
                  
                )
                
              ),
              
              div(
                
                class = "game-card-body",
                
                div(
                  class = "game-title",
                  "GTA V"
                ),
                
                div(
                  class = "game-info",
                  "Open World Action"
                ),
                
                div(
                  
                  class = "game-stat",
                  
                  span("Platform"),
                  span("PC")
                  
                )
                
              )
              
            )
            
          ),
          
          
          # FORTNITE
          column(
            
            3,
            
            div(
              
              class = "game-card",
              
              div(
                
                class = "game-image",
                
                game_image(
                  "fortnite.jpg",
                  height = "210px"
                ),
                
                div(
                  
                  class = "game-overlay",
                  
                  div(
                    class = "game-badge",
                    "BATTLE ROYALE"
                  )
                  
                )
                
              ),
              
              div(
                
                class = "game-card-body",
                
                div(
                  class = "game-title",
                  "FORTNITE"
                ),
                
                div(
                  class = "game-info",
                  "Multiplayer Battle Royale"
                ),
                
                div(
                  
                  class = "game-stat",
                  
                  span("Platform"),
                  span("PC")
                  
                )
                
              )
              
            )
            
          )
          
        ),
        
        
        # SECOND ROW
        fluidRow(
          
          # PUBG
          column(
            
            3,
            
            div(
              
              class = "game-card",
              
              div(
                
                class = "game-image",
                
                game_image(
                  "pubg.jpg",
                  height = "210px"
                ),
                
                div(
                  
                  class = "game-overlay",
                  
                  div(
                    class = "game-badge",
                    "BATTLE ROYALE"
                  )
                  
                )
                
              ),
              
              div(
                
                class = "game-card-body",
                
                div(
                  class = "game-title",
                  "PUBG"
                ),
                
                div(
                  class = "game-info",
                  "Competitive Battle Royale"
                ),
                
                div(
                  
                  class = "game-stat",
                  
                  span("Platform"),
                  span("Mobile")
                  
                )
                
              )
              
            )
            
          ),
          
          
          # CALL OF DUTY
          column(
            
            3,
            
            div(
              
              class = "game-card",
              
              div(
                
                class = "game-image",
                
                game_image(
                  "callofduty.jpg",
                  height = "210px"
                ),
                
                div(
                  
                  class = "game-overlay",
                  
                  div(
                    class = "game-badge",
                    "FPS"
                  )
                  
                )
                
              ),
              
              div(
                
                class = "game-card-body",
                
                div(
                  class = "game-title",
                  "CALL OF DUTY"
                ),
                
                div(
                  class = "game-info",
                  "Fast-Paced Shooter"
                ),
                
                div(
                  
                  class = "game-stat",
                  
                  span("Platform"),
                  span("Mobile")
                  
                )
                
              )
              
            )
            
          ),
          
          
          # FIFA
          column(
            
            3,
            
            div(
              
              class = "game-card",
              
              div(
                
                class = "game-image",
                
                game_image(
                  "fifa.jpg",
                  height = "210px"
                ),
                
                div(
                  
                  class = "game-overlay",
                  
                  div(
                    class = "game-badge",
                    "SPORTS"
                  )
                  
                )
                
              ),
              
              div(
                
                class = "game-card-body",
                
                div(
                  class = "game-title",
                  "EA SPORTS FC"
                ),
                
                div(
                  class = "game-info",
                  "Football Sports Game"
                ),
                
                div(
                  
                  class = "game-stat",
                  
                  span("Platform"),
                  span("Console")
                  
                )
                
              )
              
            )
            
          ),
          
          
          # FREE FIRE
          column(
            
            3,
            
            div(
              
              class = "game-card",
              
              div(
                
                class = "game-image",
                
                game_image(
                  "freefire.jpg",
                  height = "210px"
                ),
                
                div(
                  
                  class = "game-overlay",
                  
                  div(
                    class = "game-badge",
                    "BATTLE ROYALE"
                  )
                  
                )
                
              ),
              
              div(
                
                class = "game-card-body",
                
                div(
                  class = "game-title",
                  "FREE FIRE"
                ),
                
                div(
                  class = "game-info",
                  "Mobile Battle Royale"
                ),
                
                div(
                  
                  class = "game-stat",
                  
                  span("Platform"),
                  span("Mobile")
                  
                )
                
              )
              
            )
            
          )
          
        ),
        
        
        # ========================================================
        # CHARTS
        # ========================================================
        
        div(
          class = "section-title",
          "📈 Gaming Analytics"
        ),
        
        fluidRow(
          
          column(
            
            6,
            
            div(
              
              class = "chart-card",
              
              div(
                class = "chart-title",
                "🎮 Gaming Hours by Game"
              ),
              
              plotOutput(
                "game_hours_chart",
                height = "330px"
              )
              
            )
            
          ),
          
          column(
            
            6,
            
            div(
              
              class = "chart-card",
              
              div(
                class = "chart-title",
                "🎯 Genre Distribution"
              ),
              
              plotOutput(
                "genre_chart",
                height = "330px"
              )
              
            )
            
          )
          
        ),
        
        fluidRow(
          
          column(
            
            6,
            
            div(
              
              class = "chart-card",
              
              div(
                class = "chart-title",
                "🖥 Platform Distribution"
              ),
              
              plotOutput(
                "platform_chart",
                height = "330px"
              )
              
            )
            
          ),
          
          column(
            
            6,
            
            div(
              
              class = "chart-card",
              
              div(
                class = "chart-title",
                "😊 Player Mood"
              ),
              
              plotOutput(
                "mood_chart",
                height = "330px"
              )
              
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # ADD RECORD
      # ==========================================================
      
      tabItem(
        
        tabName = "add",
        
        div(
          class = "page-title",
          "➕ Add Gaming Record"
        ),
        
        div(
          class = "page-description",
          
          "Add a new player and save the information
          directly to the gaming CSV database."
          
        ),
        
        div(
          
          class = "image-banner",
          
          game_image(
            "character2.jpg",
            height = "260px"
          )
          
        ),
        
        div(
          
          class = "info-card",
          
          fluidRow(
            
            column(
              
              4,
              
              textInput(
                "player_name",
                "Player Name"
              ),
              
              numericInput(
                "age",
                "Age",
                20,
                min = 1
              ),
              
              selectInput(
                
                "game",
                
                "Game",
                
                choices = c(
                  "Valorant",
                  "PUBG",
                  "Minecraft",
                  "GTA V",
                  "Free Fire",
                  "Call of Duty",
                  "FIFA",
                  "Fortnite"
                )
                
              ),
              
              selectInput(
                
                "platform",
                
                "Platform",
                
                choices = c(
                  "PC",
                  "Mobile",
                  "Console"
                )
                
              )
              
            ),
            
            column(
              
              4,
              
              selectInput(
                
                "genre",
                
                "Genre",
                
                choices = c(
                  "FPS",
                  "Battle Royale",
                  "Sandbox",
                  "Action",
                  "Sports"
                )
                
              ),
              
              numericInput(
                "hours",
                "Hours Per Day",
                4,
                min = 0
              ),
              
              numericInput(
                "days",
                "Days Per Week",
                5,
                min = 0,
                max = 7
              ),
              
              numericInput(
                "session",
                "Session Minutes",
                90,
                min = 0
              )
              
            ),
            
            column(
              
              4,
              
              numericInput(
                "sleep",
                "Sleep Hours",
                7,
                min = 0
              ),
              
              numericInput(
                "screen",
                "Screen Time",
                7,
                min = 0
              ),
              
              numericInput(
                "spending",
                "Gaming Spending",
                500,
                min = 0
              ),
              
              selectInput(
                
                "mood",
                
                "Mood",
                
                choices = c(
                  "Happy",
                  "Neutral",
                  "Relaxed",
                  "Stressed"
                )
                
              )
              
            )
            
          ),
          
          br(),
          
          actionButton(
            "add_record",
            "🎮 ADD GAMING RECORD",
            class = "btn-primary"
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # MANAGE RECORDS
      # ==========================================================
      
      tabItem(
        
        tabName = "manage",
        
        div(
          class = "page-title",
          "🗂 Manage Gaming Records"
        ),
        
        div(
          class = "page-description",
          "View and delete player records stored in your CSV file."
        ),
        
        # IMPORTANT:
        # character1.jpg is used here.
        # character3.jpg was removed.
        div(
          
          class = "image-banner",
          
          game_image(
            "character1.jpg",
            height = "260px"
          )
          
        ),
        
        div(
          
          class = "info-card",
          
          selectInput(
            "delete_id",
            "Select Player ID",
            choices = NULL
          ),
          
          actionButton(
            "delete_record",
            "🗑 DELETE RECORD",
            class = "btn-primary"
          )
          
        ),
        
        DTOutput(
          "manage_table"
        )
        
      ),
      
      
      # ==========================================================
      # ANALYTICS
      # ==========================================================
      
      tabItem(
        
        tabName = "analytics",
        
        div(
          class = "page-title",
          "📊 Advanced Analytics"
        ),
        
        div(
          class = "page-description",
          
          "Discover relationships between gaming,
          sleep, screen time and spending."
          
        ),
        
        div(
          
          class = "image-banner",
          
          game_image(
            "gaming_setup.jpg",
            height = "270px"
          )
          
        ),
        
        fluidRow(
          
          column(
            
            6,
            
            div(
              
              class = "chart-card",
              
              div(
                class = "chart-title",
                "🎮 Gaming Hours vs Sleep"
              ),
              
              plotOutput(
                "sleep_chart",
                height = "380px"
              )
              
            )
            
          ),
          
          column(
            
            6,
            
            div(
              
              class = "chart-card",
              
              div(
                class = "chart-title",
                "🖥 Gaming Hours vs Screen Time"
              ),
              
              plotOutput(
                "screen_chart",
                height = "380px"
              )
              
            )
            
          )
          
        ),
        
        fluidRow(
          
          column(
            
            12,
            
            div(
              
              class = "chart-card",
              
              div(
                class = "chart-title",
                "💰 Gaming Spending by Game"
              ),
              
              plotOutput(
                "spending_chart",
                height = "400px"
              )
              
            )
            
          )
          
        )
        
      ),
      
      
      # ==========================================================
      # DATA
      # ==========================================================
      
      tabItem(
        
        tabName = "data",
        
        div(
          class = "page-title",
          "🎮 Gaming Database"
        ),
        
        div(
          class = "page-description",
          "Complete player gaming data stored in CSV format."
        ),
        
        div(
          
          class = "info-card",
          
          DTOutput(
            "gaming_table"
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# 8. SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # REACTIVE DATA
  # ==========================================================
  
  data_reactive <- reactiveVal(
    gaming_data
  )
  
  
  # ==========================================================
  # FILTERED DATA
  # ==========================================================
  
  filtered_data <- reactive({
    
    df <- data_reactive()
    
    
    # GAME
    if (
      input$filter_game != "All"
    ) {
      
      df <- df[
        df$Game == input$filter_game,
      ]
      
    }
    
    
    # GENRE
    if (
      input$filter_genre != "All"
    ) {
      
      df <- df[
        df$Genre == input$filter_genre,
      ]
      
    }
    
    
    # PLATFORM
    if (
      input$filter_platform != "All"
    ) {
      
      df <- df[
        df$Platform == input$filter_platform,
      ]
      
    }
    
    
    # MOOD
    if (
      input$filter_mood != "All"
    ) {
      
      df <- df[
        df$Mood == input$filter_mood,
      ]
      
    }
    
    
    # AGE
    if (
      input$filter_age != "All"
    ) {
      
      if (
        input$filter_age == "18-20"
      ) {
        
        df <- df[
          df$Age >= 18 &
            df$Age <= 20,
        ]
        
      }
      
      
      if (
        input$filter_age == "21-23"
      ) {
        
        df <- df[
          df$Age >= 21 &
            df$Age <= 23,
        ]
        
      }
      
      
      if (
        input$filter_age == "24+"
      ) {
        
        df <- df[
          df$Age >= 24,
        ]
        
      }
      
    }
    
    
    df
    
  })
  
  
  # ==========================================================
  # RESET FILTERS
  # ==========================================================
  
  observeEvent(
    
    input$reset_filters,
    
    {
      
      updateSelectInput(
        session,
        "filter_game",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "filter_genre",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "filter_platform",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "filter_mood",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "filter_age",
        selected = "All"
      )
      
    }
    
  )
  
  
  # ==========================================================
  # KPI - TOTAL PLAYERS
  # ==========================================================
  
  output$total_players <- renderText({
    
    nrow(
      filtered_data()
    )
    
  })
  
  
  # ==========================================================
  # KPI - AVERAGE GAMING
  # ==========================================================
  
  output$avg_hours <- renderText({
    
    df <- filtered_data()
    
    if (
      nrow(df) == 0
    ) {
      
      return("0 h")
      
    }
    
    paste0(
      
      round(
        mean(
          df$Hours_Per_Day,
          na.rm = TRUE
        ),
        1
      ),
      
      " h"
      
    )
    
  })
  
  
  # ==========================================================
  # KPI - AVERAGE SLEEP
  # ==========================================================
  
  output$avg_sleep <- renderText({
    
    df <- filtered_data()
    
    if (
      nrow(df) == 0
    ) {
      
      return("0 h")
      
    }
    
    paste0(
      
      round(
        mean(
          df$Sleep_Hours,
          na.rm = TRUE
        ),
        1
      ),
      
      " h"
      
    )
    
  })
  
  
  # ==========================================================
  # KPI - TOTAL SPENDING
  # ==========================================================
  
  output$total_spending <- renderText({
    
    df <- filtered_data()
    
    if (
      nrow(df) == 0
    ) {
      
      return("₹0")
      
    }
    
    paste0(
      
      "₹",
      
      format(
        
        round(
          sum(
            df$Gaming_Spending,
            na.rm = TRUE
          )
        ),
        
        big.mark = ","
      )
      
    )
    
  })
  
  
  # ==========================================================
  # KPI - HIGH GAMERS
  # ==========================================================
  
  output$high_players <- renderText({
    
    df <- filtered_data()
    
    sum(
      df$Hours_Per_Day >= 5,
      na.rm = TRUE
    )
    
  })
  
  
  # ==========================================================
  # GAME HOURS CHART
  # ==========================================================
  
  output$game_hours_chart <- renderPlot({
    
    df <- filtered_data()
    
    if (
      nrow(df) == 0
    ) {
      
      return(NULL)
      
    }
    
    chart_data <- df %>%
      
      group_by(Game) %>%
      
      summarise(
        
        Average_Hours =
          mean(
            Hours_Per_Day,
            na.rm = TRUE
          )
        
      )
    
    
    ggplot(
      
      chart_data,
      
      aes(
        
        x = reorder(
          Game,
          Average_Hours
        ),
        
        y = Average_Hours
        
      )
      
    ) +
      
      geom_col(
        fill = "#3b82f6",
        width = 0.65
      ) +
      
      coord_flip() +
      
      labs(
        x = NULL,
        y = "Average Hours"
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.background =
          element_rect(
            fill = "#0d1324"
          ),
        
        panel.background =
          element_rect(
            fill = "#0d1324"
          ),
        
        panel.grid =
          element_blank(),
        
        text =
          element_text(
            color = "white"
          ),
        
        axis.text =
          element_text(
            color = "#cbd5e1"
          ),
        
        axis.title =
          element_text(
            color = "#cbd5e1"
          )
        
      )
    
  })
  
  
  # ==========================================================
  # GENRE CHART
  # ==========================================================
  
  output$genre_chart <- renderPlot({
    
    df <- filtered_data()
    
    if (
      nrow(df) == 0
    ) {
      
      return(NULL)
      
    }
    
    chart_data <- df %>%
      
      count(Genre) %>%
      
      mutate(
        
        Percentage =
          n / sum(n) * 100
        
      )
    
    
    ggplot(
      
      chart_data,
      
      aes(
        
        x = 2,
        y = Percentage,
        fill = Genre
        
      )
      
    ) +
      
      geom_col(
        width = 1
      ) +
      
      coord_polar(
        theta = "y"
      ) +
      
      xlim(
        0.5,
        2.5
      ) +
      
      theme_void() +
      
      theme(
        
        plot.background =
          element_rect(
            fill = "#0d1324",
            color = NA
          ),
        
        legend.text =
          element_text(
            color = "white"
          )
        
      )
    
  })
  
  
  # ==========================================================
  # PLATFORM CHART
  # ==========================================================
  
  output$platform_chart <- renderPlot({
    
    df <- filtered_data()
    
    if (
      nrow(df) == 0
    ) {
      
      return(NULL)
      
    }
    
    chart_data <- df %>%
      
      count(Platform) %>%
      
      mutate(
        
        Percentage =
          n / sum(n) * 100
        
      )
    
    
    ggplot(
      
      chart_data,
      
      aes(
        
        x = 2,
        y = Percentage,
        fill = Platform
        
      )
      
    ) +
      
      geom_col(
        width = 1
      ) +
      
      coord_polar(
        theta = "y"
      ) +
      
      xlim(
        0.5,
        2.5
      ) +
      
      theme_void() +
      
      theme(
        
        plot.background =
          element_rect(
            fill = "#0d1324",
            color = NA
          ),
        
        legend.text =
          element_text(
            color = "white"
          )
        
      )
    
  })
  
  
  # ==========================================================
  # MOOD CHART
  # ==========================================================
  
  output$mood_chart <- renderPlot({
    
    df <- filtered_data()
    
    if (
      nrow(df) == 0
    ) {
      
      return(NULL)
      
    }
    
    chart_data <- df %>%
      
      count(Mood) %>%
      
      mutate(
        
        Percentage =
          n / sum(n) * 100
        
      )
    
    
    ggplot(
      
      chart_data,
      
      aes(
        
        x = 2,
        y = Percentage,
        fill = Mood
        
      )
      
    ) +
      
      geom_col(
        width = 1
      ) +
      
      coord_polar(
        theta = "y"
      ) +
      
      xlim(
        0.5,
        2.5
      ) +
      
      theme_void() +
      
      theme(
        
        plot.background =
          element_rect(
            fill = "#0d1324",
            color = NA
          ),
        
        legend.text =
          element_text(
            color = "white"
          )
        
      )
    
  })
  
  
  # ==========================================================
  # SLEEP CHART
  # ==========================================================
  
  output$sleep_chart <- renderPlot({
    
    df <- data_reactive()
    
    ggplot(
      
      df,
      
      aes(
        x = Hours_Per_Day,
        y = Sleep_Hours
      )
      
    ) +
      
      geom_point(
        size = 4,
        color = "#60a5fa"
      ) +
      
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "#f472b6"
      ) +
      
      labs(
        
        x = "Gaming Hours Per Day",
        y = "Sleep Hours"
        
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.background =
          element_rect(
            fill = "#0d1324"
          ),
        
        panel.background =
          element_rect(
            fill = "#0d1324"
          ),
        
        panel.grid =
          element_blank(),
        
        text =
          element_text(
            color = "white"
          ),
        
        axis.text =
          element_text(
            color = "#cbd5e1"
          ),
        
        axis.title =
          element_text(
            color = "#cbd5e1"
          )
        
      )
    
  })
  
  
  # ==========================================================
  # SCREEN TIME CHART
  # ==========================================================
  
  output$screen_chart <- renderPlot({
    
    df <- data_reactive()
    
    ggplot(
      
      df,
      
      aes(
        x = Hours_Per_Day,
        y = Screen_Time
      )
      
    ) +
      
      geom_point(
        size = 4,
        color = "#a78bfa"
      ) +
      
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "#22c55e"
      ) +
      
      labs(
        
        x = "Gaming Hours Per Day",
        y = "Screen Time"
        
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.background =
          element_rect(
            fill = "#0d1324"
          ),
        
        panel.background =
          element_rect(
            fill = "#0d1324"
          ),
        
        panel.grid =
          element_blank(),
        
        text =
          element_text(
            color = "white"
          ),
        
        axis.text =
          element_text(
            color = "#cbd5e1"
          ),
        
        axis.title =
          element_text(
            color = "#cbd5e1"
          )
        
      )
    
  })
  
  
  # ==========================================================
  # SPENDING CHART
  # ==========================================================
  
  output$spending_chart <- renderPlot({
    
    df <- data_reactive()
    
    chart_data <- df %>%
      
      group_by(Game) %>%
      
      summarise(
        
        Spending =
          sum(
            Gaming_Spending,
            na.rm = TRUE
          )
        
      )
    
    
    ggplot(
      
      chart_data,
      
      aes(
        
        x = reorder(
          Game,
          Spending
        ),
        
        y = Spending
        
      )
      
    ) +
      
      geom_col(
        fill = "#8b5cf6"
      ) +
      
      coord_flip() +
      
      labs(
        
        x = NULL,
        y = "Gaming Spending (₹)"
        
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.background =
          element_rect(
            fill = "#0d1324"
          ),
        
        panel.background =
          element_rect(
            fill = "#0d1324"
          ),
        
        panel.grid =
          element_blank(),
        
        text =
          element_text(
            color = "white"
          ),
        
        axis.text =
          element_text(
            color = "#cbd5e1"
          ),
        
        axis.title =
          element_text(
            color = "#cbd5e1"
          )
        
      )
    
  })
  
  
  # ==========================================================
  # ADD RECORD
  # ==========================================================
  
  observeEvent(
    
    input$add_record,
    
    {
      
      if (
        is.null(input$player_name) ||
        input$player_name == ""
      ) {
        
        showNotification(
          "Please enter player name.",
          type = "error"
        )
        
        return()
        
      }
      
      
      current_data <-
        data_reactive()
      
      
      new_id <-
        
        if (
          nrow(current_data) == 0
        ) {
          
          1
          
        } else {
          
          max(
            current_data$Player_ID
          ) + 1
          
        }
      
      
      new_record <- data.frame(
        
        Player_ID =
          new_id,
        
        Player_Name =
          input$player_name,
        
        Age =
          input$age,
        
        Game =
          input$game,
        
        Platform =
          input$platform,
        
        Genre =
          input$genre,
        
        Hours_Per_Day =
          input$hours,
        
        Days_Per_Week =
          input$days,
        
        Session_Minutes =
          input$session,
        
        Sleep_Hours =
          input$sleep,
        
        Screen_Time =
          input$screen,
        
        Gaming_Spending =
          input$spending,
        
        Mood =
          input$mood,
        
        stringsAsFactors =
          FALSE
        
      )
      
      
      updated_data <-
        
        rbind(
          current_data,
          new_record
        )
      
      
      write.csv(
        
        updated_data,
        
        csv_file,
        
        row.names = FALSE
        
      )
      
      
      data_reactive(
        updated_data
      )
      
      
      updateTextInput(
        
        session,
        
        "player_name",
        
        value = ""
        
      )
      
      
      showNotification(
        
        "🎮 Gaming record added successfully!",
        
        type = "message"
        
      )
      
    }
    
  )
  
  
  # ==========================================================
  # DELETE DROPDOWN
  # ==========================================================
  
  observe({
    
    df <-
      data_reactive()
    
    updateSelectInput(
      
      session,
      
      "delete_id",
      
      choices =
        df$Player_ID
      
    )
    
  })
  
  
  # ==========================================================
  # DELETE RECORD
  # ==========================================================
  
  observeEvent(
    
    input$delete_record,
    
    {
      
      req(
        input$delete_id
      )
      
      
      current_data <-
        data_reactive()
      
      
      updated_data <-
        
        current_data[
          
          current_data$Player_ID !=
            
            as.numeric(
              input$delete_id
            ),
          
        ]
      
      
      write.csv(
        
        updated_data,
        
        csv_file,
        
        row.names = FALSE
        
      )
      
      
      data_reactive(
        updated_data
      )
      
      
      showNotification(
        
        "Record deleted successfully!",
        
        type = "message"
        
      )
      
    }
    
  )
  
  
  # ==========================================================
  # MANAGE TABLE
  # ==========================================================
  
  output$manage_table <-
    
    renderDT({
      
      datatable(
        
        data_reactive(),
        
        options = list(
          
          pageLength = 8,
          
          scrollX = TRUE
          
        ),
        
        rownames = FALSE
        
      )
      
    })
  
  
  # ==========================================================
  # MAIN TABLE
  # ==========================================================
  
  output$gaming_table <-
    
    renderDT({
      
      datatable(
        
        data_reactive(),
        
        options = list(
          
          pageLength = 10,
          
          scrollX = TRUE,
          
          searching = TRUE
          
        ),
        
        rownames = FALSE
        
      )
      
    })
  
}


# ============================================================
# 9. RUN APP
# ============================================================

shinyApp(
  
  ui = ui,
  
  server = server
  
)
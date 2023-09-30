library(tidyverse)        # dplyr, ggplot2, and friends
library(broom)            # Convert models to data frames
library(marginaleffects)  # Marginal effects stuff
library(emmeans)          # Marginal effects stuff

# Visualization-related packages
library(ggtext)           # Add markdown/HTML support to text in plots
library(glue)             # Python-esque string interpolation
library(scales)           # Functions to format numbers nicely
library(gganimate)        # Make animated plots
library(patchwork)        # Combine ggplots
library(ggrepel)          # Make labels that don't overlap
library(MetBrewer)        # Artsy color palettes

# Data-related packages
library(palmerpenguins)   # Penguin data
library(WDI)              # Get data from the World Bank's API
library(countrycode)      # Map country codes to different systems
library(vdemdata)         # Use data from the Varieties of Democracy (V-Dem) project
# Install vdemdata from GitHub, not CRAN
# devtools::install_github("vdeminstitute/vdemdata")

library(showtext)
font_add_google(name = "IBM Plex Sans Condensed", family = "IBM Plex Sans Condensed")
showtext_auto()

# Helpful functions
# -------------------
# Format numbers in pretty ways
nice_number <- label_number(style_negative = "minus", accuracy = 0.01)
nice_p <- label_pvalue(prefix = c("p < ", "p = ", "p > "))

# Point-slope formula: (y - y1) = m(x - x1)
find_intercept <- function(x1, y1, slope) {
  intercept <- slope * (-x1) + y1
  return(intercept)
}

# Visualization settings
# ------------------------

# Custom ggplot theme to make pretty plots
# Get IBM Plex Sans Condensed at https://fonts.google.com/specimen/IBM+Plex+Sans+Condensed
theme_mfx <- function() {
  theme_minimal(base_family = "IBM Plex Sans Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}


# Make labels use IBM Plex Sans by default
update_geom_defaults("label", 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults(ggtext::GeomRichText, 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults("label_repel", 
                     list(family = "IBM Plex Sans Condensed"))

# Use the Johnson color palette
clrs <- met.brewer("Johnson")


# y = 2x - 1
a_line <- function(x) (2 * x) - 1

ggplot() +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_function(fun = a_line, size = 1, color = clrs[2]) +
  scale_x_continuous(breaks = -2:5, limits = c(-1, 3)) +
  scale_y_continuous(breaks = -3:9) +
  annotate(geom = "segment", x = 1, y = 1.3, xend = 1, yend = 3, color = clrs[4], size = 0.5) +
  annotate(geom = "segment", x = 1, y = 3, xend = 1.8, yend = 3, color = clrs[4], size = 0.5) +
  annotate(geom = "richtext", x = 1.4, y = 3.1, label = "Slope: **2**", vjust = 0) +
  labs(x = "x", y = "y") +
  coord_equal() +
  theme_mfx()


slope_annotations <- tibble(x = c(-0.25, 1.2, 2.4)) |> 
  mutate(y = a_line(x)) |> 
  mutate(nice_y = y + 1) |> 
  mutate(nice_label = glue("x: {x}; y: {y}<br>",
                           "Slope (dy/dx): **{2}**"))

ggplot() +
  geom_vline(xintercept = 0, size = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, size = 0.5, color = "grey50") +
  geom_function(fun = a_line, size = 1, color = clrs[2]) +
  geom_point(data = slope_annotations, aes(x = x, y = y)) +
  geom_richtext(data = slope_annotations, 
                aes(x = x, y = y, label = nice_label),
                nudge_y = 0.5) +
  scale_x_continuous(breaks = -2:5, limits = c(-1, 3)) +
  scale_y_continuous(breaks = -3:9) +
  labs(x = "x", y = "y") +
  coord_equal() +
  theme_mfx()



# y = -0.5x^2 + 5x + 5
a_parabola <- function(x) (-0.5 * x^2) + (5 * x) + 5

ggplot() +
  geom_vline(xintercept = 0, size = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, size = 0.5, color = "grey50") +
  geom_function(fun = a_parabola, size = 1, color = clrs[2]) +
  xlim(-5, 15) +
  labs(x = "x", y = "y") +
  coord_cartesian(ylim = c(-5, 20)) +
  theme_mfx()

# dy/dx = -x + 5
parabola_slope <- function(x) (-x) + 5

slope_annotations <- tibble(
  x = c(0, 3, 8)
) |> 
  mutate(y = a_parabola(x),
         slope = parabola_slope(x),
         intercept = find_intercept(x, y, slope),
         nice_slope = glue("Slope (dy/dx)<br><span style='font-size:12pt;color:{clrs[4]}'>**{slope}**</span>"))

ggplot() +
  geom_vline(xintercept = 0, size = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, size = 0.5, color = "grey50") +
  geom_function(fun = a_parabola, size = 1, color = clrs[2]) +
  geom_abline(data = slope_annotations,
              aes(slope = slope, intercept = intercept),
              size = 0.5, color = clrs[4], linetype = "21") +
  geom_point(data = slope_annotations, aes(x = x, y = y),
             size = 3, color = clrs[4]) +
  geom_richtext(data = slope_annotations, aes(x = x, y = y, label = nice_slope),
                nudge_y = 2) +
  xlim(-5, 15) +
  labs(x = "x", y = "y") +
  coord_cartesian(ylim = c(-5, 20)) +
  theme_mfx()


# u = -0.5x^2 + 5x
u_cookies <- function(x) (-0.5 * x^2) + (5 * x)

ggplot() +
  geom_vline(xintercept = 0, size = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, size = 0.5, color = "grey50") +
  geom_function(fun = u_cookies, size = 1, color = clrs[2]) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 12)) +
  labs(x = "Cookies", y = "Utility (happiness points)") +
  theme_mfx()



# du/dx = -x + 5
mu_cookies <- function(x) -x + 5

ggplot() +
  geom_vline(xintercept = 0, size = 0.5, color = "grey50") +
  geom_hline(yintercept = 0, size = 0.5, color = "grey50") +
  geom_vline(xintercept = 5, size = 0.5, 
             linetype = "21", color = clrs[3]) +
  geom_function(fun = mu_cookies, size = 1, color = clrs[5]) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 12)) +
  labs(x = "Cookies", y = "Marginal utility (additional happiness points)") +
  theme_mfx()


penguins <- penguins |> drop_na()

model_slider <- lm(body_mass_g ~ flipper_length_mm, data = penguins)
tidy(model_slider)

model_switch <- lm(body_mass_g ~ species, data = penguins)
tidy(model_switch)


model_mixer <- lm(body_mass_g ~ flipper_length_mm + bill_depth_mm + species + sex,
                  data = penguins)
tidy(model_mixer)


# Get data from the World Bank's API
wdi_raw <- WDI(country = "all", 
               indicator = c(population = "SP.POP.TOTL",
                             gdp_percapita = "NY.GDP.PCAP.KD"), 
               start = 2000, end = 2020, extra = TRUE)

# Clean up the World Bank data
wdi_2020 <- wdi_raw |> 
  filter(region != "Aggregates") |> 
  filter(year == 2020) |> 
  mutate(log_gdp_percapita = log(gdp_percapita)) |> 
  select(-region, -status, -year, -country, -lastupdated, -lending)

# Get data from V-Dem and clean it up
vdem_2020 <- vdem %>% 
  select(country_name, country_text_id, year, region = e_regionpol_6C,
         disclose_donations_ord = v2eldonate_ord, 
         public_sector_corruption = v2x_pubcorr,
         polyarchy = v2x_polyarchy, civil_liberties = v2x_civlib) %>% 
  filter(year == 2020) %>% 
  mutate(disclose_donations = disclose_donations_ord >= 3,
         disclose_donations = ifelse(is.na(disclose_donations), FALSE, disclose_donations)) %>% 
  # Scale these up so it's easier to talk about 1-unit changes
  mutate(across(c(public_sector_corruption, polyarchy, civil_liberties), ~ . * 100)) |> 
  mutate(region = factor(region, 
                         labels = c("Eastern Europe and Central Asia",
                                    "Latin America and the Caribbean",
                                    "Middle East and North Africa",
                                    "Sub-Saharan Africa",
                                    "Western Europe and North America",
                                    "Asia and Pacific")))

# Combine World Bank and V-Dem data into a single dataset
corruption <- vdem_2020 |> 
  left_join(wdi_2020, by = c("country_text_id" = "iso3c")) |> 
  drop_na(gdp_percapita)

glimpse(corruption)


plot_corruption <- corruption |> 
  mutate(highlight = civil_liberties == min(civil_liberties) | 
           civil_liberties == max(civil_liberties))


ggplot(plot_corruption, aes(x = civil_liberties, y = public_sector_corruption)) +
  geom_point(aes(color = highlight)) +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, color = clrs[1]) +
  geom_label_repel(data = filter(plot_corruption, highlight == TRUE), 
                   aes(label = country_name), seed = 1234) +
  scale_color_manual(values = c("grey30", clrs[3]), guide = "none") +
  labs(x = "Civil liberties index", y = "Public sector corruption index") +
  theme_mfx()


model_simple <- lm(public_sector_corruption ~ civil_liberties,
                   data = corruption)
tidy(model_simple)


ggplot(plot_corruption, aes(x = civil_liberties, y = public_sector_corruption)) +
  geom_point(aes(color = highlight)) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, color = clrs[2]) +
  geom_label_repel(data = filter(plot_corruption, highlight == TRUE), 
                   aes(label = country_name), seed = 1234) +
  scale_color_manual(values = c("grey30", clrs[3]), guide = "none") +
  labs(x = "Civil liberties index", y = "Public sector corruption index") +
  theme_mfx()


model_sq <- lm(public_sector_corruption ~ civil_liberties + I(civil_liberties^2),
               data = corruption)
tidy(model_sq)


# Extract the two civil_liberties coefficients
civ_lib1 <- tidy(model_sq) |> filter(term == "civil_liberties") |> pull(estimate)
civ_lib2 <- tidy(model_sq) |> filter(term == "I(civil_liberties^2)") |> pull(estimate)

# Make a little function to do the math
civ_lib_slope <- function(x) civ_lib1 + (2 * civ_lib2 * x)

civ_lib_slope(c(25, 55, 80))


tangents <- model_sq |> 
  augment(newdata = tibble(civil_liberties = c(25, 55, 80))) |> 
  mutate(slope = civ_lib_slope(civil_liberties),
         intercept = find_intercept(civil_liberties, .fitted, slope)) |> 
  mutate(nice_label = glue("Civil liberties: {civil_liberties}<br>",
                           "Fitted corruption: {nice_number(.fitted)}<br>",
                           "Slope: **{nice_number(slope)}**"))

ggplot(corruption, aes(x = civil_liberties, y = public_sector_corruption)) +
  geom_point(color = "grey30") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, color = clrs[4]) +
  geom_abline(data = tangents, aes(slope = slope, intercept = intercept), 
              size = 0.5, color = clrs[2], linetype = "21") +
  geom_point(data = tangents, aes(x = civil_liberties, y = .fitted), size = 4, shape = 18, color = clrs[2]) +
  geom_richtext(data = tangents, aes(x = civil_liberties, y = .fitted, label = nice_label), nudge_y = -7) +
  labs(x = "Civil liberties index", y = "Public sector corruption index") +
  theme_mfx()

model_sq |> 
  marginaleffects(newdata = datagrid(civil_liberties = c(25, 55, 80)),
                  eps = 0.001)
model_sq |> 
  emtrends(~ civil_liberties, var = "civil_liberties",
           at = list(civil_liberties = c(25, 55, 80)),
           delta.var = 0.001)


model_sq |> 
  emtrends(~ civil_liberties, var = "civil_liberties",
           at = list(civil_liberties = c(25, 55, 80)),
           delta.var = 0.001) |> 
  test()


# Automatic plot from marginaleffects::plot_cme()
mfx_marginaleffects_auto <- plot_cme(model_sq, 
                                     variables = "civil_liberties", 
                                     condition = "civil_liberties") +
  labs(x = "Civil liberties", y = "Marginal effect of civil liberties on public sector corruption",
       subtitle = "Created automatically with marginaleffects::plot_cme()") +
  theme_mfx()

# Piece all the geoms together manually with results from marginaleffects::marginaleffects()
mfx_marginaleffects <- model_sq |> 
  marginaleffects(newdata = datagrid(civil_liberties = 
                                       seq(min(corruption$civil_liberties), 
                                           max(corruption$civil_liberties), 0.1)),
                  eps = 0.001) |> 
  ggplot(aes(x = civil_liberties, y = dydx)) +
  geom_vline(xintercept = 42, color = clrs[3], size = 0.5, linetype = "24") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, fill = clrs[1]) +
  geom_line(size = 1, color = clrs[1]) +
  labs(x = "Civil liberties", y = "Marginal effect of civil liberties on public sector corruption",
       subtitle = "Calculated with marginaleffects()") +
  theme_mfx()

# Piece all the geoms together manually with results from emmeans::emtrends()
mfx_emtrends <- model_sq |> 
  emtrends(~ civil_liberties, var = "civil_liberties",
           at = list(civil_liberties = 
                       seq(min(corruption$civil_liberties), 
                           max(corruption$civil_liberties), 0.1)),
           delta.var = 0.001) |> 
  as_tibble() |> 
  ggplot(aes(x = civil_liberties, y = civil_liberties.trend)) +
  geom_vline(xintercept = 42, color = clrs[3], size = 0.5, linetype = "24") +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.1, fill = clrs[1]) +
  geom_line(size = 1, color = clrs[1]) +
  labs(x = "Civil liberties", y = "Marginal effect of civil liberties on public sector corruption",
       subtitle = "Calculated with emtrends()") +
  theme_mfx()

mfx_marginaleffects_auto | mfx_marginaleffects | mfx_emtrends

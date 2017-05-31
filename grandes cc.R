library(httr)
library(rvest)
library(knitr)
library(kableExtra)
library(ggalt)
library(statebins)
library(hrbrthemes)
library(tidyverse)
library(usmap)
library(epidata)
library(docxtractr)

options(knitr.table.format = "html")
update_geom_font_defaults(font_rc_light, size = 2.75)

closings <- list(
  kmart = "https://www.bostonglobe.com/metro/2017/01/05/the-full-list-kmart-stores-closing-around/4kJ0YVofUWHy5QJXuPBAuM/story.html",
  sears = "https://www.bostonglobe.com/metro/2017/01/05/the-full-list-sears-stores-closing-around/yHaP6nV2C4gYw7KLhuWuFN/story.html",
  macys = "https://www.bostonglobe.com/metro/2017/01/05/the-full-list-macy-stores-closing-around/6TY8a3vy7yneKV1nYcwY7K/story.html",
  jcp = "https://www.bostonglobe.com/business/2017/03/17/the-full-list-penney-stores-closing-around/vhoHjI3k75k2pSuQt2mZpO/story.html"
)

saved_pgs <- "saved_store_urls.rds"

if (file.exists(saved_pgs)) {
  pgs <- read_rds(saved_pgs)
} else {
  pgs <- map(closings, GET)
  write_rds(pgs, saved_pgs)
}

map(pgs, content) %>%
  map(html_table) %>%
  walk(~glimpse(.[[1]]))

map(pgs, content) %>%
  map(html_table) %>%
  map(~.[[1]]) %>%
  map_df(select, abb=3, .id = "store") -> closings


map(pgs, content) %>%
  map(html_table) %>%
  map(~.[[1]]) %>%
  map_df(select, abb=3, .id = "store") -> closings




count(closings, abb) %>%
  left_join(data_frame(name = state.name, abb = state.abb)) %>%
  left_join(usmap::statepop, by = c("abb"="abbr")) %>%
  mutate(per_capita = (n/pop_2015) * 1000000) %>%
  select(name, n, per_capita) -> closings_by_state

arrange(closings_by_state, per_capita) %>%
  mutate(name = factor(name, name)) %>%
  ggplot(aes(per_capita, name)) +
  geom_lollipop(horizontal = TRUE) +
  scale_x_continuous(name="Closings per capita (1MM)", expand=c(0,0), limits=c(0,7.5)) +
  geom_label(aes(label=sprintf("%1.2f (n=%d)", per_capita, n)),
             hjust=0, nudge_x=0.1, label.size=0) +
  labs(y=NULL) +
  theme_ipsum_rc(grid="X") +
  theme(axis.text.x=element_blank())

statebins_continuous(closings_by_state, state_col="name", value_col="n", 
                     legend_title = "Total closings per-state")


arrange(closings_by_state, n) %>%
  mutate(name = factor(name, name)) %>%
  ggplot(aes(n, name)) +
  geom_lollipop(horizontal = TRUE) +
  scale_x_continuous(name="Total closings", expand=c(0,0), limits=c(0,30)) +
  geom_label(aes(label=n), hjust=0, nudge_x=0.25, label.size=0) +
  labs(y=NULL) +
  theme_ipsum_rc(grid="X") +
  theme(axis.text.x=element_blank())


arrange(closings_by_state, desc(n)) %>%
  select(State=name, `Total Stores Closing`=n, `Per Capita (1MM)`=per_capita) %>%
  kable(align = "lrr", caption="Retail Stores Closing (sorted by total stores)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = "condensed")


arrange(closings_by_state, name) %>%
  select(State=name, `Total Stores Closing`=n, `Per Capita (1MM)`=per_capita) %>%
  kable(align = "lrr", caption="Retail Stores Closing (sorted by state name)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = "condensed")

pg <- read_html("https://www.bls.gov/lau/stalt16q4.htm")

html_nodes(pg, "table#alternmeas16\\:IV") %>% 
  html_table(header = TRUE, fill = TRUE) %>%
  .[[1]] %>% 
  docxtractr::assign_colnames(1) %>% 
  rename(name=State) %>% 
  as_data_frame() %>% 
  slice(2:52) %>% 
  type_convert() %>% 
  left_join(closings_by_state, by="name") %>% 
  filter(!is.na(n)) -> with_unemp

ggplot(with_unemp, aes(per_capita, `U-6`)) +
  geom_label(aes(label=name), fill="#8c96c6", color="white", size=3.5, family=font_rc) +
  scale_x_continuous(limits=c(-0.125, 6.75)) +
  labs(x="Closings per-capita (1MM)", 
       y="BLS Labor Underutilization (U-6 rate)",
       title="Per-capita store closings compared to current BLS U-6 Rate") +
  theme_ipsum_rc(grid="XY")



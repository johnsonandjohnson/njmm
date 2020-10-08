# Libraries
pacman::p_load(DBI, dplyr, ggplot2)

# Connect to Log DB
con <- dbConnect(RSQLite::SQLite(), "logs/shinylogs.sqlite")

# Sessions Tracking
sessions_tbl <- dbReadTable(con, "session")

ggplot(sessions_tbl, aes(x = as.Date(server_connected), y = ..count..)) +
  geom_bar() +
  labs(
    x = "Date", y = "Number of Visitors",
    title = "Visitors over Time"
  )

ggplot(sessions_tbl, aes(x = user, y = ..count..)) +
  geom_bar(color = "black", aes(fill = user)) +
  labs(
    x = "Date", y = "Number of Visitors",
    title = "Number Visits by Different Visitors"
  ) +
  theme(legend.position = "none")


# Inputs Tracking
inputs_tbl <- dbReadTable(con, "inputs")

ggplot(inputs_tbl, aes(x = name, y = ..count..)) +
  geom_bar(color = "black", aes(fill = name)) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Input Widget", y = "Number of Uses",
    title = "Number Uses by Input Widget"
  ) +
  theme(legend.position = "none")

# Outputs Tracking
outputs_tbl <- dbReadTable(con, "outputs")

ggplot(outputs_tbl, aes(x = name, y = ..count..)) +
  geom_bar(color = "black", aes(fill = name)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Output Widget", y = "Number of Uses",
    title = "Number Uses by Output Widget"
  ) +
  theme(legend.position = "none")

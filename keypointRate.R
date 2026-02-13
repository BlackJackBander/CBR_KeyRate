# ======================================================
# 1. Загрузка и подготовка данных
# ======================================================

library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)

# Чтение данных
df <- read.csv2("INF_F17_09_2013_T12_02_2026.csv", stringsAsFactors = FALSE, fileEncoding = "Windows-1251")

# Обработка данных
df_clean <- df %>%
  # Переименовываем колонки для удобства
  rename(
    Дата = 1,
    Ставка = 2,
    Инфляция = 3,
    Цель = 4
  ) %>%
  mutate(
    # Парсим дату в формате DD.MM.YYYY
    Дата = dmy(Дата),  # dmy = день-месяц-год
    # Замена запятой на точку и преобразование в число
    Ставка = as.numeric(str_replace(Ставка, ",", ".")),
    Инфляция = as.numeric(str_replace(Инфляция, ",", ".")),
    Цель = as.numeric(str_replace(Цель, ",", "."))
  ) %>%
  # Удаляем строки с NA
  filter(!is.na(Дата), !is.na(Ставка)) %>%
  # Сортируем по возрастанию даты
  arrange(Дата) %>%
  # Превращаем в tsibble
  as_tsibble(index = Дата)

# Проверка
glimpse(df_clean)
head(df_clean)

# ======================================================
# 2. График исходных рядов: ставка и инфляция
# ======================================================

p1 <- df_clean %>%
  ggplot(aes(x = Дата)) +
  geom_line(aes(y = Ставка, color = "Ключевая ставка"), size = 1) +
  geom_line(aes(y = Инфляция, color = "Инфляция (г/г)"), size = 1) +
  geom_hline(aes(yintercept = 4, linetype = "Цель 4%"), color = "darkgreen", size = 0.8) +
  scale_color_manual(values = c("Ключевая ставка" = "steelblue", "Инфляция (г/г)" = "coral")) +
  scale_linetype_manual(name = "", values = "dashed") +
  labs(
    title = "Ключевая ставка и инфляция в России",
    y = "%",
    x = "Дата",
    color = "Показатель",
    linetype = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# ======================================================
# 3. Построение модели TSLM (fable)
# ======================================================

# Обучающая выборка — до последнего известного месяца (02.2026)
train <- df_clean %>% filter(Дата < ymd("2026-03-01"))

# Модель: Ставка ~ Инфляция + тренд + лаг?
# Добавим лаг 1 месяца и лаг инфляции, чтобы модель "чувствовала" динамику
train <- train %>%
  mutate(
    Ставка_лаг1 = lag(Ставка, 1),
    Инфляция_лаг1 = lag(Инфляция, 1)
  ) %>%
  filter(!is.na(Ставка_лаг1))

# Спецификация модели TSLM
fit <- train %>%
  model(
    tslm_model = TSLM(Ставка ~ Инфляция + Инфляция_лаг1 + Ставка_лаг1 + trend())
  )

# Отчёт по модели
report(fit)

# ======================================================
# 4. Прогноз на следующий месяц (03.2026)
# ======================================================

# Создаём новые данные для прогноза
future <- new_data(train, 1) %>%
  mutate(
    Инфляция = 5.59,   # последнее известное значение (12.2025)
    Инфляция_лаг1 = 5.59,
    Ставка_лаг1 = 16.0 # последняя ставка
  )

# Прогноз
fc <- fit %>% forecast(new_data = future)

# ======================================================
# 5. Строим простой график (03.2026)
# ======================================================

# Вывод прогноза
print(fc)

history_base <- df_clean[df_clean$Date >= as.Date("2024-01-01"), ]
history_base$Date <- as.Date(history_base$Date)
history_base <- as.data.frame(history_base)

forecast_val <- as.numeric(fc$.mean)

plot(x = c(1, 2), y = c(16, forecast_val), 
     type = "b", 
     col = c("blue", "red"),
     xlab = "", ylab = "Ставка (%)",
     xaxt = "n",
     main = paste("Прогноз ставки на март 2026:", round(forecast_val, 1), "%"),
     ylim = c(10, 20))
axis(1, at = 1:2, labels = c("Фев 2026\n(факт)", "Мар 2026\n(прогноз)"))

# Встановлення оновлення етапу 2

## Що змінюється

1. Прибирається ручна нумерація з підзаголовків.
2. Усі графіки отримують однакову висоту 860 px.
3. Графіки займають ширшу область сторінки.
4. Годинний графік показує 90 днів.
5. Чотиригодинний графік показує один рік.
6. Додається швидке Parquet-сховище Binance.
7. Додаються інтервали `5m`, `15m`, `30m`, `1h`, `2h`, `4h`, `6h`, `12h`, `1d`.
8. Додається вибіркове читання хвилинних даних через DuckDB.
9. Додаються контрольні ряди Bybit `4h` і `1d`.
10. До книги додається порівняльний графік Binance і Bybit.

## Файли

```text
02-data-acquisition.qmd
R/book_charts.R
R/binance_data_fast.R
R/bybit_data.R
scripts/02_validate_and_build_charts.R
scripts/03_update_binance_fast.R
scripts/04_get_bybit_comparison.R
```

## Необхідні пакети

У консолі RStudio:

```r
renv::install(c(
  "data.table",
  "DBI",
  "duckdb"
))
```

Після встановлення перезапустіть R-сесію.

## Перший запуск швидкого сховища

```r
source(
  "scripts/03_update_binance_fast.R",
  encoding = "UTF-8"
)
```

Перший запуск перетворює наявні перевірені ZIP-архіви на Parquet. Він може тривати довше за наступні запуски.

## Контрольні дані Bybit

```r
source(
  "scripts/04_get_bybit_comparison.R",
  encoding = "UTF-8"
)
```

## Оновлення окремих HTML-графіків

```r
source(
  "scripts/02_validate_and_build_charts.R",
  encoding = "UTF-8"
)
```

## Рендеринг книги

У терміналі з кореня проєкту:

```bash
quarto render
```

## Перевірка хвилинної вибірки

Після запуску швидкого сховища:

```r
btc_1m_sample = btc_query_1m(
  start_time = "2026-01-01 00:00:00",
  end_time = "2026-01-08 00:00:00"
)

nrow(btc_1m_sample)
head(btc_1m_sample)
```

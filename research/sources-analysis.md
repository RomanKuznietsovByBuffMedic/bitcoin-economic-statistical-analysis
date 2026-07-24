---
title: "Аналіз джерел для проєкту Bitcoin"
date: "2026-07-21"
lang: uk-UA
---

# 1. Призначення документа

Цей файл зберігає повний проміжний аналіз бібліотеки для проєкту
`bitcoin-economic-statistical-analysis`.

Він потрібен як резервний журнал рішень:

- які джерела вже додані до проєкту ChatGPT;
- яку унікальну функцію виконує кожне основне джерело;
- які книги з локальної бібліотеки були переглянуті;
- які матеріали є дублями або слабшими альтернативами;
- які джерела варто підключати лише під конкретну статистичну задачу;
- як покриті баєсівські методи;
- які вільні слоти залишаються в межах ліміту 25 джерел.

Важливо: цей документ не є бібліографією самої книги. До `references.bib`
треба додавати лише ті джерела, які реально цитуються у тексті дослідження.

# 2. Поточне рішення

Оптимальна структура джерел ChatGPT:

- 11 постійних методологічних джерел;
- до 14 вільних або тимчасових слотів;
- загальний ліміт - 25 джерел.

Не потрібно заповнювати всі 25 слотів наперед. Тимчасові джерела додаються
під конкретну гіпотезу, модель, пакет або розділ.

# 3. Поточні 11 постійних джерел ChatGPT

| № | Джерело | Основна функція |
|---:|---|---|
| 1 | Frank de Jong, Barbara Rindi - *The Microstructure of Financial Markets* | Ордери, спред, ліквідність, limit order book, формування ціни, торгові витрати |
| 2 | Oliver Linton - *Financial Econometrics: Models and Methods* | Фінансова економетрика, передбачуваність доходностей, ефективність ринку, ризик |
| 3 | Ruey S. Tsay - *Analysis of Financial Time Series* | Основна книга з фінансових часових рядів, ARMA, GARCH, нелінійність, високочастотні дані |
| 4 | Robert H. Shumway, David S. Stoffer - *Time Series Analysis and Its Applications* | Загальні часові ряди, ARIMA, state-space, HMM, спектральний аналіз, приклади в R |
| 5 | Christian Francq, Jean-Michel Zakoian - *GARCH Models* | Строга теорія GARCH, QMLE, діагностика, асиметричні й багатовимірні моделі |
| 6 | Marcos López de Prado - *Advances in Financial Machine Learning* | Leakage, purged cross-validation, embargo, feature importance, backtesting |
| 7 | Wolfgang Karl Härdle, Léopold Simar - *Applied Multivariate Statistical Analysis* | PCA, факторний аналіз, кластеризація, copulae, багатовимірні методи |
| 8 | Simon Rogers, Mark Girolami - *A First Course in Machine Learning* | Теоретичні основи ML, Bayesian inference, MCMC, Gaussian processes, mixture models |
| 9 | Bradley Boehmke, Brandon Greenwell - *Hands-On Machine Learning with R* | Практичний ML у R, preprocessing, resampling, regularization, дерева та ансамблі |
| 10 | Paul Roback, Julie Legler - *Beyond Multiple Linear Regression* | GLM, логістична й пуассонівська регресія, multilevel models, реалізація в R |
| 11 | Raquel Prado, Marco A. R. Ferreira, Mike West - *Time Series: Modeling, Computation, and Inference* | Баєсівські часові ряди, dynamic models, state-space, MCMC, SMC, stochastic volatility |

# 4. Покриття баєсівських методів

Баєсівський напрям уже представлений достатньо для старту.

## 4.1 Основне джерело

**Prado, Ferreira, West - *Time Series: Modeling, Computation, and Inference*.**

Книга покриває:

- prior, likelihood і posterior;
- Bayesian AR та ARMA;
- dynamic linear models;
- state-space models;
- sequential updating і filtering;
- MCMC;
- Sequential Monte Carlo;
- Markov switching;
- stochastic volatility;
- Bayesian VAR;
- багатовимірні динамічні моделі.

Можливі застосування до Bitcoin:

- прогнозний розподіл доходності;
- оцінювання невизначеності прогнозу;
- прихована волатильність;
- параметри, що змінюються з часом;
- ймовірність ринкового режиму;
- порівняння GARCH із Bayesian stochastic volatility.

## 4.2 Допоміжні джерела

- **Rogers, Girolami** - загальна логіка Bayesian ML, MAP, Laplace approximation, Metropolis-Hastings, Gibbs sampling, Bayesian mixtures, Gaussian processes.
- **Shumway, Stoffer** - Bayesian analysis для linear Gaussian state-space models.
- **Tsay** - stochastic volatility, nonlinear state-space і Markov switching у фінансовому контексті.
- **Francq, Zakoian** - класична альтернатива для порівняння з GARCH і volatility models.

Окрему загальну книгу на кшталт *Bayesian Data Analysis* зараз додавати не потрібно.

# 5. Додатковий довідник з computational intelligence

**Springer Handbook of Computational Intelligence**, за редакцією
Janusza Kacprzyka та Witolda Pedrycza, варто зберігати локально як енциклопедію.

Корисні частини:

| Частина | Можливе застосування |
|---|---|
| Evolutionary Computation | Пошук параметрів, відбір ознак, багатокритеріальна оптимізація |
| Neural Networks | Нелінійний прогноз доходності або волатильності, класифікація режимів |
| Hybrid Systems | Поєднання режимної моделі, прогнозної моделі та оптимізації |
| Fuzzy Logic | Нечітка класифікація ризику або режимів |
| Rough Sets | Пошук правил і відбір ознак |
| Swarm Intelligence | Альтернативне налаштування параметрів |

Рішення:

- не додавати весь довідник до постійних джерел ChatGPT;
- не читати послідовно від початку до кінця;
- підключати лише конкретний розділ, коли виникне відповідна задача;
- цитувати конкретний розділ, а не весь том.

# 6. Скориговані рішення щодо локальної бібліотеки

Початковий звіт NotebookLM корисний як інвентар, але його рекомендації
потребують критичної перевірки. Він часто оцінював книгу за наявністю
фінансового прикладу, а не за унікальною методологічною роллю.

## 6.1 Постійні або вже додані

| Джерело | Рішення |
|---|---|
| Rogers, Girolami | Залишити постійно |
| Boehmke, Greenwell | Залишити постійно |
| Härdle, Simar, 2019 | Залишити постійно |
| Roback, Legler | Залишити постійно |
| Prado, Ferreira, West | Залишити постійно як основне Bayesian time-series джерело |

## 6.2 Тимчасові джерела під конкретну задачу

| Джерело | Коли підключати |
|---|---|
| Agresti - *Foundations of Linear and Generalized Linear Models* | Коли потрібна глибша теорія GLM |
| Hosmer, Lemeshow - *Applied Logistic Regression*, 3rd ed. | Для конкретної задачі бінарної класифікації й діагностики |
| Dobson, Barnett - *An Introduction to Generalized Linear Models* | Як альтернативне теоретичне джерело з GLM |
| Rodriguez - GLM lecture notes | Для швидкої довідки, якщо бракує стислого пояснення |
| Torgo - *Data Mining with R* | Для конкретного фінансового ML або forecasting кейсу |
| Robert H. Frank - *Microeconomics and Behavior* | Для попиту, стимулів і поведінки учасників |
| Achen - *Garbage-Can Regressions* | Для критики перенасичених регресій і некоректного вибору змінних |
| MOSR.pdf | Для структурування дослідницької роботи |
| TVMSL2_MIK.pdf | Як локальна довідка з теорії ймовірностей та математичної статистики |
| MMTC.pdf | Для мікроекономічного контексту, якщо реально використовується |
| MVA.pdf | Для локального пояснення багатовимірного аналізу |
| AM.pdf | Для окремої теми ризику або моделей разорення |
| Braun, Murdoch | Якщо знадобиться базова реалізація власних алгоритмів у R |
| Zhang, Ma - *Ensemble Machine Learning* | Лише для окремого дослідження ансамблів |
| Berk - *Statistical Learning from a Regression Perspective* | Лише якщо потрібен альтернативний статистичний погляд на ML |

## 6.3 Залишити лише в локальному архіві

### Дублювання часових рядів

- Brockwell, Davis - *Introduction to Time Series and Forecasting*;
- Wei - *Time Series Analysis: Univariate and Multivariate Methods*;
- Box, Jenkins et al. - *Time Series Analysis: Forecasting and Control*;
- Cryer, Chan - *Time Series Analysis with Applications in R*;
- Metcalfe, Cowpertwait - *Introductory Time Series with R*;
- Bisgaard, Kulahci - *Time Series Analysis and Forecasting by Example*;
- Coghlan - *A Little Book of R for Time Series*;
- Rossiter - *Time Series Analysis with R*.

Причина: поточний набір Tsay, Shumway-Stoffer, Linton і Francq-Zakoian
вже закриває базові, прикладні й спеціалізовані питання часових рядів.

### Базові або застарілі книги з R

- Crawley - *The R Book*;
- Dalgaard - *Introductory Statistics with R*;
- Gardener - *Beginning R*;
- Saiz, Gonzalez, Gil - *Introduction to Data Analysis in R*;
- Zhao - *R and Data Mining*.

Причина: частина матеріалу базова, частина прив'язана до старих workflow,
а практичний ML у R уже представлений Boehmke-Greenwell.

### Вузькі матеріали

- Copas, 1989;
- Hosmer-Copas goodness-of-fit;
- Copas_test;
- Allison - measures of fit;
- Altham - дискретна статистика;
- Hilbe - negative binomial regression;
- OptimalCutpoints;
- PSU LASSO notes;
- PSU Ridge notes;
- ECML 2004 proceedings.

Такі джерела варто підключати лише під конкретну модель або статистичний тест.

### Дублікати та старі видання

- другий файл Box-Jenkins;
- старе видання Härdle-Simar;
- Hosmer-Lemeshow, 2nd edition;
- старе ISLR 2013, якщо доступне новіше видання;
- будь-які повні дублікати за SHA-256.

# 7. Дисципліни, які зараз представлені

| Дисципліна | Основні джерела |
|---|---|
| Фінансова економетрика | Linton |
| Фінансові часові ряди | Tsay |
| Загальні часові ряди | Shumway, Stoffer |
| GARCH і волатильність | Francq, Zakoian |
| Ринкова мікроструктура | de Jong, Rindi |
| Фінансовий ML і backtesting | López de Prado |
| Багатовимірна статистика | Härdle, Simar |
| Загальний ML | Rogers, Girolami |
| Практичний ML у R | Boehmke, Greenwell |
| GLM і multilevel models | Roback, Legler |
| Bayesian time series | Prado, Ferreira, West |

# 8. Які дисципліни не треба додавати наперед

Не потрібно зараз постійно додавати:

- ще одну загальну книгу з ARIMA;
- ще одну загальну книгу з GARCH;
- deep learning без конкретної задачі;
- fuzzy logic або swarm intelligence без гіпотези;
- on-chain analysis лише тому, що він пов'язаний із Bitcoin;
- sentiment analysis без конкретного набору текстових даних;
- документацію R-пакетів;
- матеріали про торгові точки входу без сформульованої стратегії;
- технічні джерела про протокол Bitcoin, якщо вони вже є у бібліографії роботи.

# 9. Правило додавання нового джерела

Нове джерело додається до ChatGPT лише тоді, коли воно відповідає хоча б
одній із умов:

1. закриває суттєву прогалину в методології;
2. потрібне для поточного статистичного питання;
3. містить точні формули, таблиці або сторінки, які треба аналізувати;
4. не має стабільної відкритої онлайн-версії;
5. має унікальну роль і не дублює наявне ядро.

Ключове питання:

> Яке конкретне рішення в проєкті стане кращим завдяки цьому джерелу?

# 10. Резерв слотів

Після 11 постійних джерел залишається до 14 слотів.

Рекомендований тимчасовий блок:

1. стаття під поточну гіпотезу;
2. джерело про конкретну модель;
3. документація реально використаного пакета;
4. джерело для validation або risk assessment;
5. емпіричне дослідження відповідного ринку;
6. до 9 додаткових слотів як резерв.

# 11. Зауваження щодо початкового звіту NotebookLM

Початковий звіт нижче збережено без скорочення як історичний інвентар.

Його не слід трактувати як остаточний список, тому що в ньому були такі
систематичні проблеми:

- завищена цінність кількох взаємозамінних книг із часових рядів;
- автоматичне припущення задач "купувати або продавати";
- надто рання рекомендація sentiment analysis, on-chain аналізу і HFT;
- трактування документації пакета як самостійного методологічного джерела;
- рекомендація моделей через наявність фінансового прикладу;
- недостатнє врахування вже наявних Tsay, Shumway-Stoffer, Linton,
  Francq-Zakoian і López de Prado.

---

# Додаток A. Початковий аналіз 46 джерел від NotebookLM

Для вашого проєкту з економічного та статистичного аналізу Bitcoin було проаналізовано 46 джерел. Нижче наведено детальний огляд кожного з них, згрупований за категоріями для зручності.

### Аналіз джерел для проєкту Bitcoin

| Назва файла | Справжня назва, автори, рік | Основні теми | Що нового для проєкту | Дублювання | Рекомендація | Обґрунтування | Упевненість |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **! Brockwell - Intro to Time Series (2002)** | *Introduction to Time Series and Forecasting*, P. Brockwell, R. Davis (2002) | GARCH, нелінійні моделі, моделювання волатильності | Теоретичні основи станціонарності GARCH | №15, №23, №26 | Локальний архів | Класичний підручник, але є новіші видання (напр. №15) | Висока |
| **! Wei - Time-Series-Analysis** | *Time Series Analysis: Univariate and Multivariate Methods*, William W.S. Wei (2006) | Багатовимірні часові ряди, спектральний аналіз | Математичний апарат векторних процесів | №15, №26 | Локальний архів | Корисний для глибокої теорії, але перекривається сучасними R-посібниками | Середня |
| **!! Agresti - Foundations... (2015)** | *Foundations of Linear and Generalized Linear Models*, Alan Agresti (2015) | GLM, категоріальні дані, логістична регресія | Моделювання ймовірності стрибків ціни (бінарні події) | - | Тимчасове | Глибока база для GLM, корисно для класифікації ринкових станів | Висока |
| **!! Copas1989...** | *Unweighted Sum of Squares Test for Proportions*, J. B. Copas (1989) | Тести пропорцій | Специфічний статистичний тест | №11 | Локальний архів | Вузькоспеціалізована стаття | Висока |
| **!! Hosmer, Lemeshow (3rd ed)** | *Applied Logistic Regression (3rd ed)*, D. Hosmer, S. Lemeshow (2013) | Логістична регресія, діагностика моделей | Сучасні методи оцінки якості класифікації | №44 (застаріле) | Постійне | Ключове для побудови моделей "купувати/продавати" | Висока |
| **!! Hosmer-Copas-gof_logistic** | *Goodness-of-fit tests for logistic regression*, D. Hosmer et al. (1997) | Тести адекватності логістичної регресії | Перевірка точності бінарних прогнозів | - | Локальний архів | Методологічне уточнення до №5 | Висока |
| **!! Roback - Beyond Multiple Linear Regression** | *Beyond Multiple Linear Regression: Appl GLM & Multilevel Models in R*, P. Roback, J. Legler (2021) | Багаторівневі моделі, R-практикум | Ієрархічне моделювання (напр. дані з різних бірж) | - | Постійне | Найактуальніше джерело з прикладами коду в R | Висока |
| **!! Rodriguez - GLM lecture-notes** | *Generalized Linear Models (Lecture Notes)*, G. Rodriguez | Теорія GLM | Стислий виклад математики GLM | №3, №28 | Тимчасове | Зручно для швидкої довідки по формулах | Середня |
| **!! Zhang, Ma - Ensemble Machine Learning (2012)** | *Ensemble Machine Learning: Methods and Applications*, C. Zhang, Y. Ma (2012) | Ансамблеві методи, бустинг, випадкові ліси | Поєднання багатьох моделей для стабільного прогнозу ціни | №20, №22 | Постійне | Фундаментальне для ML-частини проєкту | Висока |
| **!!! Coghlan - a-little-book-of-r-for-time-series** | *A Little Book of R For Time Series*, Avril Coghlan | Основи часових рядів в R | Швидкий старт для початкової візуалізації | №26, №34 | Тимчасове | Дуже базовий рівень | Висока |
| **!!! Copas_test** | Специфічні матеріали тестування Копаса | Статистичне тестування | Додаткові методи валідації | №4 | Локальний архів | Вузька тема | Середня |
| **!!!! Rossiter R_ts** | *Time Series Analysis with R*, D.G. Rossiter (2009) | TSA в R, сезонність | Практичні скрипти для аналізу трендів | №26, №34 | Тимчасове | Добре структуровані нотатки | Висока |
| **!!!! Saiz, Gonzalez, Gil - Intro to Data Analysis in R** | *Introduction to Data Analysis in R*, Saiz et al. (2020) | Обробка великих даних, RStudio | Сучасний workflow обробки даних | №22, №39 | Постійне | Актуальні практики 2020 року | Висока |
| **Girolami & Rogers (2017)** | *A First Course in Machine Learning*, S. Rogers, M. Girolami (2017) | Основи ML, баєсівські методи | Математичне розуміння алгоритмів | №38 | Тимчасове | Якісна база для початкового ML | Висока |
| **Box, Jenkins (2015)** | *Time Series Analysis: Forecasting and Control (5th ed)*, Box et al. (2015) | ARIMA, ARCH/GARCH, блок-схеми побудови моделей | "Золотий стандарт" для GARCH та тестів одиничного кореня | №23 | Постійне | Найповніша база з часових рядів у списку | Висока |
| **AM.pdf** | *Основи актуаруної математики*, В.О. Кофанов (2005) | Теорія ризику, моделі разорення | Моделювання ризику критичних втрат капіталу | - | Локальний архів | Матеріал викладача ДНУ; специфіка — страхування життя | Висока |
| **Achen - Garbage-Can Regressions (2004)** | *Garbage-Can Regressions*, Christopher H. Achen (2004) | Критика регресійного аналізу | Застереження щодо перенасичення моделей змінними | - | Локальний архів | Методологічне застереження | Середня |
| **Allison - Measures of Fit...** | *Measures of Fit for Logistic Regression*, Paul Allison | Метрики якості моделей | Альтернативні R-квадрат для Bitcoin-моделей | №5, №18 | Тимчасове | Зручні слайди для вибору метрик | Середня |
| **Altham - Cambridge** | Нотатки Кембриджу з дискретної статистики | Аналіз таблиць спряженості | Статистична залежність між різними активами | - | Локальний архів | Вузька математична база | Низька |
| **Berk - Statistical Learning (2016)** | *Statistical Learning from a Regression Perspective*, Richard Berk (2016) | Випадкові ліси, бустинг у контексті регресії | Прогнозування Bitcoin як задачі статистичного навчання | №22 | Постійне | Поєднує класичну статистику та ML | Висока |
| **Bisgaard, Kulahci** | *Time Series Analysis and Forecasting by Example*, Bisgaard et al. (2011) | Прикладна TSA, ARIMA на реальних даних | Набір кейсів (прикладів) для Bitcoin | №15, №26 | Тимчасове | Цінний через "навчання на прикладах" | Висока |
| **Boehmke, Greenwell - Hands-On ML with R** | *Hands-On Machine Learning with R*, Boehmke & Greenwell (2020) | XGBoost, Deep Learning, інтерпретація моделей | Практична імплементація складних моделей для крипторинку | №13, №39 | Постійне | Один із найкращих сучасних практикумів в R | Висока |
| **Box, Jenkins - TSA (5th edn)** | **ДУБЛІКАТ №15** | - | - | №15 | Локальний архів | Повторення | Висока |
| **Braun, Murdoch (2016)** | *A First Course in Statistical Programming with R*, Braun & Murdoch (2016) | Програмування функцій, оптимізація | Написання власних алгоритмів для Bitcoin-даних | №25 | Тимчасове | Основи R-програмування | Висока |
| **Crawley - The R Book** | *The R Book*, Michael Crawley | Повний овідник R, фінансова статистика | База даних функцій для маніпуляцій з цінами | №29 | Постійне | Універсальний довідник | Висока |
| **Cryer, Chan - Time Series Analysis (2008)** | *Time Series Analysis With Applications in R*, Cryer & Chan (2008) | GARCH моделі, нелінійність, R-код | Готові скрипти для GARCH-M та інших модифікацій волатильності | №15, №34 | Постійне | Пряма відповідність темі Bitcoin | Висока |
| **Dalgaard - Introd Statistics with R** | *Introductory Statistics with R*, Peter Dalgaard | Базова статистика | Чистка та базова підготовка даних | №27 | Локальний архів | Занадто простий рівень | Висока |
| **Dobson, Barnett - An Intro to GLM** | *An Introduction to Generalized Linear Models*, Dobson & Barnett (2018) | Теорія GLM, часові ряди | Математичне обґрунтування моделей | №3, №28 | Тимчасове | Класичний підручник | Висока |
| **Gardener - Beginning R (2012)** | *Beginning R*, Mark Gardener (2012) | Основи R, графіки | Базова візуалізація | №25 | Локальний архів | Морально застарів у порівнянні з №22 | Висока |
| **Hilbe - Negative binom regr** | *Negative Binomial Regression*, Joseph Hilbe | Моделі лічильних даних | Аналіз кількості транзакцій Bitcoin за блок | - | Локальний архів | Дуже вузька тема | Висока |
| **MMTC.pdf** | *Мікроекономіка та теорія споживання*, Є.В. Карнаух | Теорія корисності, попит Маршалла | Економічне обґрунтування попиту на Bitcoin | №36 | Тимчасове | Матеріал викладача ДНУ (Карнаух) | Висока |
| **MOSR.pdf** | *Основи наукознавства (лекції)*, Є.В. Карнаух | Методологія досліджень | Структурування Bitcoin-проєкту як наукової роботи | - | Тимчасове | Методологічна база викладача ДНУ | Висока |
| **MVA.pdf** | *Multivariate Descriptive Analysis (лекції)*, Є.В. Карнаух | Матричний аналіз, EDA | Багатовимірний аналіз крипто-портфелів | №40 | Тимчасове | Конспект лекцій викладача ДНУ | Висока |
| **Metcalfe - Introductory time series with R** | *Introductory Time Series with R*, Metcalfe & Cowpertwait (2009) | Спектральний аналіз, волатильність | Аналіз циклів Bitcoin | №26 | Постійне | Дуже якісний прикладний підручник | Висока |
| **OptimalCutpoints.pdf** | *OptimalCutpoints: R Package for Selection of Cutpoints* | Пошук оптимальних порогів (cutpoints) | Визначення точок входу в ринок (thresholds) | - | Тимчасове | Опис пакету R, корисно для трейдинг-стратегій | Середня |
| **Robert H. Frank - ISE Microeconomics (2020)** | *Microeconomics and Behavior*, Robert Frank (2020) | Теорія ігор, інформаційна асиметрія | Поведінкові аспекти Bitcoin (FOMO, теорія ігор) | №31 | Постійне | Сучасна економічна теорія для Bitcoin | Висока |
| **TVMSL2_MIK.pdf** | *Теория вероятностей и матстатистика*, В.Н. Турчин (2018) | Тести гіпотез, регресія, теорія ймовірностей | Математично строга перевірка гіпотез щодо Bitcoin | - | Постійне | Фундаментальний підручник професора ДНУ | Висока |
| **Tibshirani - Intro to stat learning** | *An Introduction to Statistical Learning*, James, Witten, Hastie, Tibshirani (2013) | Регуляризація, деревовидні моделі | Основи фінансового прогнозування (Smarket приклад) | №20, №22 | Тимчасове | Світовий бестселер з ML, але Boehmke (№22) новіший | Висока |
| **Torgo - Data Mining with R (2nd Ed)** | *Data Mining with R: Learning with Case Studies*, Luis Torgo (2017) | Торгові стратегії, прогнозування фінансів | Готовий фреймворк для тестування торгових стратегій | №41 | Постійне | Містить прямі кейси з фінансового прогнозування | Висока |
| **Härdle, Simar - Applied Multivariate (2019)** | *Applied Multivariate Statistical Analysis (5th ed)*, Härdle & Simar (2019) | Факторний аналіз, PCA, кластеризація | Визначення латентних факторів ціни Bitcoin | №42 (старше) | Постійне | Провідне видання з багатовимірної статистики | Висока |
| **Zhao - R_and_data_mining** | *R and Data Mining: Examples and Case Studies*, Yanchang Zhao (2012) | Text mining, аналіз асоціацій | Аналіз взаємозв'язків Bitcoin з новинами | №39, №43 | Локальний архів | Трохи застарілі кейси 2012 року | Висока |
| **applied multivariate statistics...** | **ДУБЛІКАТ №40 (ймовірно старе видання)** | - | - | №40 | Локальний архів | Див. №40 | Висока |
| **ecml04.pdf** | *Machine Learning: ECML 2004* | Наукові статті з ML | Історичний розвиток алгоритмів | №9 | Локальний архів | Застаріле для Bitcoin | Середня |
| **Hosmer-Lemeshow (2nd edition)** | **ДУБЛІКАТ №5 (застаріле)** | - | - | №5 | Локальний архів | Див. новішу версію (№5) | Висока |
| **psu.edu - LASSO (!!)** | Нотатки Penn State University по LASSO | LASSO регресія | Відбір найбільш значущих факторів ціни | №38 | Локальний архів | Частина №38 | Висока |
| **psu_edu - ridge regression (!!!)** | Нотатки Penn State University по Ridge | Ridge регресія | Боротьба з мультиколінеарністю факторів | №38 | Локальний архів | Частина №38 | Висока |

---

### Підсумкові рекомендації

#### 10 найкорисніших кандидатів для проєкту Bitcoin:
1.  **Box, Jenkins, et al. (2015)** — база для ARIMA та аналізу волатильності.
2.  **Boehmke & Greenwell (2020)** — найкраща практика сучасного ML в R.
3.  **Torgo (2017)** — готові кейси фінансового трейдингу в R.
4.  **Härdle & Simar (2019)** — глибокий багатовимірний аналіз активів.
5.  **Cryer & Chan (2008)** — прикладний GARCH для фінансових рядів.
6.  **Roback & Legler (2021)** — сучасне моделювання складних структур даних.
7.  **Турчин В.М. (2018)** — теоретична база для перевірки статистичних гіпотез.
8.  **Robert Frank (2020)** — мікроекономічне обґрунтування поведінки ринку.
9.  **Zhang & Ma (2012)** — ансамблеве навчання (boosting/bagging) для точності.
10. **Berk (2016)** — погляд на статистичне навчання через призму регресії.

#### Дисципліни, яких бракує в джерелах:
*   **On-chain аналіз:** специфічні метрики блокчейну (hash rate, активні адреси, UTXO).
*   **NLP / Sentiment Analysis:** автоматичний аналіз новин та Twitter для прогнозування настроїв.
*   **High-frequency trading (HFT):** аналіз мікроструктури ринку Bitcoin на рівні мілісекунд (джерела переважно орієнтовані на денні/тижневі дані).
*   **Крипто-економіка:** специфічні моделі як Stock-to-Flow (S2F).

#### Списки за категоріями:
*   **Дублікати:** №23 (Box Jenkins), №42 (Hardle Simar), №44 (Hosmer).
*   **Застарілі видання:** Brockwell (2002), Gardener (2012), Zhao (2012), Hosmer 2nd ed.
*   **Матеріали для ручної перевірки (матеріали викладачів):** AM.pdf (Кофанов), MMTC.pdf (Карнаух), MOSR.pdf (Карнаух), MVA.pdf (Карнаух), TVMSL2_MIK.pdf (Турчин). Вони містять цінну локальну базу, але потребують адаптації під Bitcoin-контекст.

---

# Додаток B. Підсумковий короткий список

## Постійні джерела

1. de Jong, Rindi
2. Linton
3. Tsay
4. Shumway, Stoffer
5. Francq, Zakoian
6. López de Prado
7. Härdle, Simar
8. Rogers, Girolami
9. Boehmke, Greenwell
10. Roback, Legler
11. Prado, Ferreira, West

## Локально зберігати, але не тримати постійно у ChatGPT

- Springer Handbook of Computational Intelligence;
- усі додаткові підручники з часових рядів;
- спеціалізовані GLM і logistic regression джерела;
- Torgo;
- Frank;
- матеріали викладачів;
- вузькі статті й документацію пакетів;
- старі видання та дублікати.

## Наступне рішення

Поки не додавати нові постійні джерела. Наступне джерело обирати лише після
формулювання конкретної статистичної гіпотези або практичного етапу.

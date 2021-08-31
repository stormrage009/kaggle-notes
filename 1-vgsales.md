---
output:
  html_document: default
  pdf_document: default
---

# Video Games Sales {#Video-Games-Sales}



## 准备 {#game-sales-intro}


```r
file <- "D:/Tools/Rwork/0.Study R/kaggle-project/data/vgsales.csv"
df <- read_csv(file)
str(df)
```

```
## spec_tbl_df [19,600 x 9] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ Rank         : num [1:19600] 1 2 3 4 5 6 7 8 9 10 ...
##  $ Name         : chr [1:19600] "Wii Sports" "Super Mario Bros." "Counter-Strike: Global Offensive" "Mario Kart Wii" ...
##  $ Platform     : chr [1:19600] "Wii" "NES" "PC" "Wii" ...
##  $ Publisher    : chr [1:19600] "Nintendo" "Nintendo" "Valve" "Nintendo" ...
##  $ Developer    : chr [1:19600] "Nintendo EAD" "Nintendo EAD" "Valve Corporation" "Nintendo EAD" ...
##  $ Critic_Score : num [1:19600] 7.7 10 8 8.2 8.6 10 8 9.4 9.1 8.6 ...
##  $ User_Score   : num [1:19600] 8 8.2 7.5 9.1 4.7 7.8 8.8 8.8 8.1 9.2 ...
##  $ Total_Shipped: num [1:19600] 82.9 40.2 40 37.3 36.6 ...
##  $ Year         : num [1:19600] 2006 1985 2012 2008 2017 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   Rank = col_double(),
##   ..   Name = col_character(),
##   ..   Platform = col_character(),
##   ..   Publisher = col_character(),
##   ..   Developer = col_character(),
##   ..   Critic_Score = col_double(),
##   ..   User_Score = col_double(),
##   ..   Total_Shipped = col_double(),
##   ..   Year = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
head(df, 3)
```

```
## # A tibble: 3 x 9
##    Rank Name  Platform Publisher Developer Critic_Score User_Score Total_Shipped
##   <dbl> <chr> <chr>    <chr>     <chr>            <dbl>      <dbl>         <dbl>
## 1     1 Wii ~ Wii      Nintendo  Nintendo~          7.7        8            82.9
## 2     2 Supe~ NES      Nintendo  Nintendo~         10          8.2          40.2
## 3     3 Coun~ PC       Valve     Valve Co~          8          7.5          40  
## # ... with 1 more variable: Year <dbl>
```

数据共包括11列，具体变量说明如下表所示：


```r
table_var <- read_excel("data-intro.xlsx", sheet = 3)
kable(table_var, align = "c") %>% 
  kable_classic()
```

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:center;"> 变量 </th>
   <th style="text-align:center;"> 说明 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Rank </td>
   <td style="text-align:center;"> 销量排名 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Name </td>
   <td style="text-align:center;"> 游戏名称 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Platform </td>
   <td style="text-align:center;"> 发型平台 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Publisher </td>
   <td style="text-align:center;"> 发行商 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Develooper </td>
   <td style="text-align:center;"> 开发商 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Critic_Score </td>
   <td style="text-align:center;"> 从业人评分 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> User_Score </td>
   <td style="text-align:center;"> 用户评分 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Total_shipped </td>
   <td style="text-align:center;"> 总出货量 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Year </td>
   <td style="text-align:center;"> 发型年份 </td>
  </tr>
</tbody>
</table>

## 缺失值处理


```r
summary(df)
```

```
##       Rank           Name             Platform          Publisher        
##  Min.   :    1   Length:19600       Length:19600       Length:19600      
##  1st Qu.: 4899   Class :character   Class :character   Class :character  
##  Median : 9798   Mode  :character   Mode  :character   Mode  :character  
##  Mean   : 9799                                                           
##  3rd Qu.:14698                                                           
##  Max.   :19598                                                           
##                                                                          
##   Developer          Critic_Score      User_Score     Total_Shipped    
##  Length:19600       Min.   : 0.800   Min.   : 1.000   Min.   : 0.0100  
##  Class :character   1st Qu.: 6.100   1st Qu.: 6.300   1st Qu.: 0.0500  
##  Mode  :character   Median : 7.300   Median : 7.200   Median : 0.1600  
##                     Mean   : 7.035   Mean   : 6.995   Mean   : 0.5511  
##                     3rd Qu.: 8.200   3rd Qu.: 8.000   3rd Qu.: 0.4600  
##                     Max.   :10.000   Max.   :10.000   Max.   :82.9000  
##                     NA's   :9631     NA's   :17377                     
##       Year     
##  Min.   :1977  
##  1st Qu.:2004  
##  Median :2008  
##  Mean   :2008  
##  3rd Qu.:2012  
##  Max.   :2020  
## 
```

```r
sum(is.na(df))
```

```
## [1] 27010
```

```r
n_miss(df)
```

```
## [1] 27010
```

数据中有27010个缺失值，而缺失值主要存在与`Critic_Score`和`User_Score`，主要原因在于并不是每个用户和从业者都会对游戏进行评分。
需要对其进行一些处理，未打分的我们认为其打分为5.0分，即使用5.0代替所有缺失值。


```r
# 采用每一列的众数替换该列的缺失值
df <- df %>% 
  map_dfc(~replace_na(.x, rstatix::get_mode(.x)[1]))
```

## 描述性分析

描述性统计是一个统计范围，它应用各种技术来描述和总结任何数据集，并研究观察到的数据的一般行为，以促进问题的解决。这可以通过频率表、图形和集中趋势的度量来完成，例如平均值、中位数、众数、离散度量（例如标准偏差、百分位数和四分位数）。

创建`Percent`列计算公式如下式\@ref(eq:per)：

\begin{align}
  p = 100 * \frac{Freq_x}{\sum_{i=0}^{n}(Freq_i)}
  (\#eq:per)
\end{align}

式中：$p$为百分比；$Freq_x$为元素$x$出现的频数；$\sum_{i=0}^{n}(Freq_i)$为所有元素的总频数。


### 频数分布 {#sec:Frequency-Distribution}



#### 哪一年的游戏总销量最高

#### 游戏总销量排名

#### 游戏平台销量排名

#### 开发商排名

#### 发行商总销量排名


### 中心分布指标 {#sec:Central-Trend-Measures}

#### 平均值

#### 中数

#### 众数

### Separating Measures

### Dispersion Measures


## 探索性分析

### 世界最畅销游戏

### 游戏发型平台

### 游戏发行商发型游戏书

### 每年世界销量

## 其它分析

### 用户打分与游戏销量关系

### 从业者打分与游戏销量关系

### 用户与从业者分别最喜欢哪个发行商

### 用户与从业者分别最喜欢哪个发行商

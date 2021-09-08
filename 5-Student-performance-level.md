---
output:
  html_document: default
  pdf_document: default
---

# Student performance level classification {#scoreClass}



## 数据变量说明 {#sec:data-intro-edu}

[变量说明](https://www.kaggle.com/aljarah/xAPI-Edu-Data)。

变量中最重要的的为`Class`学生等级变量，是我们建模的目标变量。


```r
edudata <- read_csv("data/xAPI-Edu-Data.csv")
head(edudata)
```

```
## # A tibble: 6 x 17
##   gender NationalITy PlaceofBirth StageID    GradeID SectionID Topic Semester
##   <chr>  <chr>       <chr>        <chr>      <chr>   <chr>     <chr> <chr>   
## 1 M      KW          KuwaIT       lowerlevel G-04    A         IT    F       
## 2 M      KW          KuwaIT       lowerlevel G-04    A         IT    F       
## 3 M      KW          KuwaIT       lowerlevel G-04    A         IT    F       
## 4 M      KW          KuwaIT       lowerlevel G-04    A         IT    F       
## 5 M      KW          KuwaIT       lowerlevel G-04    A         IT    F       
## 6 F      KW          KuwaIT       lowerlevel G-04    A         IT    F       
## # ... with 9 more variables: Relation <chr>, raisedhands <dbl>,
## #   VisITedResources <dbl>, AnnouncementsView <dbl>, Discussion <dbl>,
## #   ParentAnsweringSurvey <chr>, ParentschoolSatisfaction <chr>,
## #   StudentAbsenceDays <chr>, Class <chr>
```


## 描述性分析 {#sec:descriptive-analysis}

### 封装绘图函数


```r
fun_bar <- function(data, xlab, fillc, pos, xname, yname, legend){
  data %>% 
    group_by({{xlab}}) %>% # dplyr中的自定函数参数需要使用{{}}括起来
    mutate(count = n()) %>% 
      ggplot(aes(reorder({{xlab}}, count), count, fill = {{fillc}})) +
      geom_col(position = pos) + #pos = "stack" or "fill"
      labs(x = xname, y = yname) +
      coord_flip() +
      theme_bw() +
      guides(fill = guide_legend(title = legend))
}
```

### 不同教育程度的学生选择课程主题


```r
p1 <- fun_bar(data = edudata, xlab = Topic, fillc = StageID,
             pos = "stack", xname = "Topic", yname = "Student_Count",
             legend = "教育程度")
p2 <- fun_bar(data = edudata, xlab = Topic, fillc = StageID,
             pos = "fill", xname = "Topic", yname = "Per_Student_Count",
             legend = "教育程度")
p1/p2
```

<div class="figure" style="text-align: center">
<img src="5-Student-performance-level_files/figure-html/school-level-1.png" alt="不同教育程度的学生选择课程主题" width="672" />
<p class="caption">(\#fig:school-level)不同教育程度的学生选择课程主题</p>
</div>
由图\@ref(fig:school-level)可以看出：

- 课程主题最多的为IT、French和Arabic，其中选择IT的课程主题的学员远高于其他课。

- 无论哪种教育程度，IT、Science、Math和English四种课程都是必修的（三种颜色都有）。

### 不同课程主题监护人情况

这部分主要针对家长的情况进行分析，了解父母对学员学习的不同情况。对应在数据集中的变量为`Relation`。


```r
p3 <- fun_bar(data = edudata, xlab = Topic, fillc = Relation,
              pos = "stack", xname = "Topic", yname = "Student_count",
              legend = "监护人情况")
p4 <- fun_bar(data = edudata, xlab = Topic, fillc = Relation,
              pos = "fill", xname = "Topic", yname = "Student_count",
              legend = "监护人情况")
p3/p4
```

<div class="figure" style="text-align: center">
<img src="5-Student-performance-level_files/figure-html/relation-1.png" alt="不同课程主题监护人情况" width="672" />
<p class="caption">(\#fig:relation)不同课程主题监护人情况</p>
</div>

由图\@ref(fig:relation)可以看出：

- 总体而言，监护人为父亲的较多。其中，IT和Math课程中，负责人为父亲的超过75%。

- French课程，监护人大多数为母亲，占70%左右。

### 不同课程学生学习成绩


```r
p5 <- fun_bar(data = edudata, xlab = Topic, fillc = Class,
              pos = "stack", xname = "Topic", yname = "Student_count",
              legend = "学生成绩")
p6 <- fun_bar(data = edudata, xlab = Topic, fillc = Class,
              pos = "fill", xname = "Topic", yname = "Student_count",
              legend = "学生成绩")
p5/p6
```

<div class="figure" style="text-align: center">
<img src="5-Student-performance-level_files/figure-html/class-1.png" alt="不同课程学生学习成绩" width="672" />
<p class="caption">(\#fig:class)不同课程学生学习成绩</p>
</div>
由图\@ref(fig:class):

- 所有课程中，只有Biology课程中，属于高水平的学生数超过了50%。

- 在Geology课程中，没有低水平的学生。

### 不同教室学生成绩水平


```r
p7 <- fun_bar(data = edudata, xlab = SectionID, fillc = Class,
              pos = "stack", xname = "Section_ID", yname = "Student_count",
              legend = "学生成绩")
p8 <- fun_bar(data = edudata, xlab = SectionID, fillc = Class,
              pos = "fill", xname = "Section_ID", yname = "Student_count",
              legend = "学生成绩")
p7/p8
```

<div class="figure" style="text-align: center">
<img src="5-Student-performance-level_files/figure-html/score-section-1.png" alt="不同教室学生成绩水平" width="672" />
<p class="caption">(\#fig:score-section)不同教室学生成绩水平</p>
</div>

由图\@ref(fig:score-section)可以看出：

- 在A班的学生最多，C班的学生最少。

- C班的低水平成绩的学生相对较多，其它两个班级的成绩水平基本一致。

### 不同学期、不同成绩水平与监护人的关系


```r
# 封装函数，去掉坐标轴翻转
fun_bar2 <- function(data, xlab, fillc, pos, xname, yname, legend){
  data %>% 
    group_by({{xlab}}) %>% # dplyr中的自定函数参数需要使用{{}}括起来
    mutate(count = n()) %>% 
      ggplot(aes(reorder({{xlab}}, count), count, fill = {{fillc}})) +
      geom_col(position = pos) + #pos = "stack" or "fill"
      labs(x = xname, y = yname) +
      theme_bw() +
      guides(fill = guide_legend(title = legend))
}
```



```r
p9 <- fun_bar2(edudata, Semester, Relation, pos = "stack",
              xname = "Semester", yname = "Student_count",
              legend = "监护人情况")

p10 <- fun_bar2(edudata, Semester, Relation, pos = "fill",
              xname = "Semester", yname = "per_Student_count",
              legend = "监护人情况")

p11 <- fun_bar2(edudata, Class, Relation, pos = "stack",
               xname = "Class", yname = "Student_count",
               legend = "监护人情况")

p12 <- fun_bar2(edudata, Class, Relation, pos = "fill",
               xname = "Class", yname = "per_Student_count",
               legend = "监护人情况")
(p9|p10) / (p11|p12)
```

<div class="figure" style="text-align: center">
<img src="5-Student-performance-level_files/figure-html/semester-1.png" alt="不同学期、不同成绩水平与监护人的关系" width="672" />
<p class="caption">(\#fig:semester)不同学期、不同成绩水平与监护人的关系</p>
</div>
由图\@ref(fig:semester)可知：

- 第一学期父亲作为监护人的学生数比第二学期多。

- 总体看，成绩水平较高的学生中，监护人为母亲的比较多；其它水平均是父亲较多。

### 家长是否回答调查问卷、成绩水平与家长对学校是否满意的关系


```r
p13 <- fun_bar2(edudata, ParentAnsweringSurvey, ParentschoolSatisfaction, 
                pos = "stack", xname = "ParentAnsweringSurvey",
                yname = "Student_count", legend = "是否满意")
p14 <- fun_bar2(edudata, ParentAnsweringSurvey, ParentschoolSatisfaction, 
                pos = "fill", xname = "ParentAnsweringSurvey",
                yname = "Per_Student_count", legend = "是否满意")

p15 <- fun_bar2(edudata, Class, ParentschoolSatisfaction, 
                pos = "stack", xname = "Class",
                yname = "Student_count", legend = "是否满意")
p16 <- fun_bar2(edudata, Class, ParentschoolSatisfaction, 
                pos = "fill", xname = "Class",
                yname = "Per_Student_count", legend = "是否满意")
(p13|p14)/(p15|p16)
```

<div class="figure" style="text-align: center">
<img src="5-Student-performance-level_files/figure-html/surey-class-1.png" alt="家长是否回答调查问卷、成绩水平与家长对学校是否满意的关系" width="672" />
<p class="caption">(\#fig:surey-class)家长是否回答调查问卷、成绩水平与家长对学校是否满意的关系</p>
</div>
由图\@ref(fig:surey-class)可以看出：

- 有超过一半的家长回答了问卷，其中，回答问卷的家长大部分对学校满意，而未回答问卷的则大部分对学校不满。

- 成绩越高，家长对学校越满意。

### 性别、逃课次数与学生成绩水平的关系


```r
p17 <- fun_bar2(edudata, gender, Class, 
                pos = "stack", xname = "Gender",
                yname = "Student_count", legend = "成绩水平")
p18 <- fun_bar2(edudata, gender, Class, 
                pos = "fill", xname = "Gender",
                yname = "Per_Student_count", legend = "成绩水平")

p19 <- fun_bar2(edudata, StudentAbsenceDays, Class, 
                pos = "stack", xname = "Class",
                yname = "Student_count", legend = "成绩水平")
p20 <- fun_bar2(edudata, StudentAbsenceDays, Class, 
                pos = "fill", xname = "Class",
                yname = "Per_Student_count", legend = "成绩水平")
(p17|p18)/(p19|p20)
```

<div class="figure" style="text-align: center">
<img src="5-Student-performance-level_files/figure-html/gender-absence-1.png" alt="性别、逃课次数与学生成绩水平的关系" width="672" />
<p class="caption">(\#fig:gender-absence)性别、逃课次数与学生成绩水平的关系</p>
</div>
由图\@ref(fig:gender-absence)可知：

- 女生比男生数量少很多，但高水平成绩的人数明显比男生多；中水平成绩男女比例基本持平。

- 逃课超过7天的的学生基本无法取得好的成绩。

### 举手次数和参加讨论次数与成绩水平关系


```r
fun_bar3 <- function(data, xlab, ylab, fillc, xname, yname){
  data %>% 
    group_by({{xlab}}) %>% 
    summarise(Mcount = mean({{ylab}})) %>% 
     ggplot(aes({{xlab}}, Mcount, fill = {{fillc}})) +
      geom_col() + 
      labs(x = xname, y = yname) +
      theme_bw() +
      theme(legend.position = "none")
}
```



```r
edudata$Class <- factor(edudata$Class, c("H", "M", "L"), ordered = TRUE)
p21 <- fun_bar3(data = edudata, xlab = Class, ylab = raisedhands, 
                fillc = Class, "成绩水平", "平均举手次数" )

p22 <- fun_bar3(data = edudata, xlab = Class, ylab = Discussion, 
                fillc = Class, "成绩水平", "平均参与讨论次数" )
p21|p22
```

<div class="figure" style="text-align: center">
<img src="5-Student-performance-level_files/figure-html/raisedhands-discuss-1.png" alt="举手次数和参加讨论次数与成绩水平关系" width="672" />
<p class="caption">(\#fig:raisedhands-discuss)举手次数和参加讨论次数与成绩水平关系</p>
</div>
由图\@ref(fig:raisedhands-discuss)可知： 举手次数和参与讨论次数越多，成绩水平越高。

***

## 模型建立

### 回归树模型建立

#### 分层抽样


```r
set.seed(1234)
# 按照数据目标8:2进行分层抽样，返回矩阵形式的抽样索引
index <- createDataPartition(edudata$Class, p = 0.8, list = F)
train <- edudata[index, ]
test <- edudata[-index, ]

# 建立回归树模型
rpart_model <- rpart(Class ~., data = train)
# type = "class"指定预测结果是具体的某个类别
prep_rp <- predict(rpart_model, test[-17], type = "class")
confusionMatrix(prep_rp, test$Class)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  H  M  L
##          H 18  3  0
##          M  9 29  3
##          L  1 10 22
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7263          
##                  95% CI : (0.6252, 0.8128)
##     No Information Rate : 0.4421          
##     P-Value [Acc > NIR] : 1.882e-08       
##                                           
##                   Kappa : 0.5806          
##                                           
##  Mcnemar's Test P-Value : 0.05103         
## 
## Statistics by Class:
## 
##                      Class: H Class: M Class: L
## Sensitivity            0.6429   0.6905   0.8800
## Specificity            0.9552   0.7736   0.8429
## Pos Pred Value         0.8571   0.7073   0.6667
## Neg Pred Value         0.8649   0.7593   0.9516
## Prevalence             0.2947   0.4421   0.2632
## Detection Rate         0.1895   0.3053   0.2316
## Detection Prevalence   0.2211   0.4316   0.3474
## Balanced Accuracy      0.7990   0.7320   0.8614
```

```r
prp(rpart_model)
```

<img src="5-Student-performance-level_files/figure-html/unnamed-chunk-5-1.png" width="672" style="display: block; margin: auto;" />



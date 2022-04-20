--- 
title: "Kaggle数据项目实战"
author: 
  - Lee
date: "2022-04-20"
site: bookdown::bookdown_site
documentclass: elegantbook
bibliography: 
 - book.bib
 - packages.bib
lot: yes
lof: yes
colorlinks: yes
toccolor: Maroon
link-citations: yes
nocite: '@*'    # 将正文未引用的参考文献也列在书中
mathspec: yes
graphics: yes
subject: "基于 elegantbook 文类的 bookdown 模版"
keywords:
  - elegantbook
  - bookdown
  - pandoc
  - R
hyperrefoptions:
- linktoc=all
- pdfstartview=FitH
github-repo: XiangyunHuang/ElegantBookdown
classoption: 
 - lang=cn
 - 11pt
 - scheme=chinese
 - chinesefont=nofont
 - citestyle=gb7714-2015
 - bibstyle=gb7714-2015
description: "最初看到 elegantbook 做的书籍样式很漂亮，就想把它引入到 bookdown 中，遂定制了此模版。在此基础上，做了迁移和扩展的工作，融合了 LaTeX (精美)、Pandoc (简洁) 和 R (强大) 的特性。This is a bookdown template based on ElegantBook. The output format for this template is bookdown::gitbook and bookdown::pdf_book."
---

\mainmatter


# 简介 {-}

Kaggle成立于2010年，是一个进行数据发掘和预测竞赛的在线平台。从公司的角度来讲，可以提供一些数据，进而提出一个实际需要解决的问题；从参赛者的角度来讲，他们将组队参与项目，针对其中一个问题提出解决方案，最终由公司选出的最佳方案可以获得5K-10K美金的奖金。

除此之外，Kaggle官方每年还会举办一次大规模的竞赛，奖金高达一百万美金，吸引了广大的数据科学爱好者参与其中。从某种角度来讲，大家可以把它理解为一个众包平台，类似国内的猪八戒。但是不同于传统的低层次劳动力需求，Kaggle一直致力于解决业界难题，因此也创造了一种全新的劳动力市场——不再以学历和工作经验作为唯一的人才评判标准，而是着眼于个人技能，为顶尖人才和公司之间搭建了一座桥梁。

## 已有 Block {#theorem-block}

::: {.lemma #chf-pdf}
For any two random variables $X_1$, $X_2$, they both have the same probability distribution if and only if
$$\varphi _{X_1}(t)=\varphi _{X_2}(t)$$
:::

::: {.theorem #chf-sum}
If $X_1$, ..., $X_n$ are independent random variables, and $a_1$, ..., $a_n$ are some constants, then the characteristic function of the linear combination $S_n=\sum_{i=1}^na_iX_i$ is
$$\varphi _{S_{n}}(t)=\prod_{i=1}^n\varphi _{X_i}(a_{i}t)=\varphi _{X_{1}}(a_{1}t)\cdots \varphi _{X_{n}}(a_{n}t)$$
:::

::: {.proposition #unnamed-chunk-1}
The distribution of the sum of independent Poisson random variables $X_i \sim \mathrm{Pois}(\lambda_i),\: i=1,2,\cdots,n$ is $\mathrm{Pois}(\sum_{i=1}^n\lambda_i)$.
:::



## 数学公式 {#math-formular}

[^load-bm]: <https://github.com/ElegantLaTeX/ElegantBook/blob/6ab10beda81252f0b478e05fa926199301347e4a/elegantbook.cls#L884>

数学公式加粗可能是最常见的需求之一， **elegantbook** 宏包提供的文类 `elegantbook.cls` 已经调用了 **bm** 宏包 [^load-bm]。有了 **bm** 宏包，就可以使用 **bm** 宏包提供的 `\bm{}` 命令，而不需要调 `\boldsymbol{}` 加粗希腊字母，如将  $\alpha$ （正常）加粗为 $\bm{\alpha}$（粗体）。为了在 HTML 网页中显示加粗效果，则还不够，默认情况下， MathJax 是不认识 `\bm{}` 命令的，所以需要在 `header.html` 自定义 `\bm{}` 命令：

```html
<script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      TeX: {
        Macros: {
          bm: ["{\\boldsymbol #1}",1],
        }
      }
    });
</script>
```

进一步地，使用常用的 3 个取消符号 $\bcancel{///}$ 需要在 `header.html` 添加 JS 库 `cancel.js`，

```html
<script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      TeX: {
        Macros: {
          bm: ["{\\boldsymbol #1}",1],
        },
        extensions: ["cancel.js"]
      }
    });
</script>
```

并在 preamble.tex 文件中添加一行代码加载 **cancel** 宏包

```latex
\usepackage[makeroom]{cancel}
```

## 自定义 block {#custom-block}

基于 Pandoc 自定义 block 是一件很有意思的事情，目前不想让模版过于复杂，仅给出几个最常用的例子。如何自定义可以去看谢益辉的新书 <https://bookdown.org/yihui/rmarkdown-cookbook/custom-blocks.html>。

[要做的还有很多]{.todo}

::: {.rmdwarn data-latex="{警告}"}
这是警告
:::

::: {.rmdtip data-latex="{提示}"}
这是提示
:::

:::: {.rmdnote data-latex="{注意}"}
这是注意
::::

::: {.rmdinfo}
普通说明
:::



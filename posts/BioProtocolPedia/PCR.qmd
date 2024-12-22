---
title: "聚合酶链式反应（polymerase chain reaction，PCR）"
date: "2024-10-14"
date-modified: today
categories: [protocols, DNA]
---

聚合酶链式反应：体外DNA复制，模板DNA，引物（3'-OH末端）、热稳定DNA聚合酶、dNTP、含Mg2+的缓冲液

## 试剂和仪器

### 试剂

1.  热稳定DNA聚合酶（*taq*酶，-20℃）。
2.  10×PCR扩增缓冲液。
3.  25 mmol/L $MgCl_2$。
4.  4种脱氧单核苷三磷酸（dNTPs）混合贮存液（20 mmol/L,pH=8.0,-20℃）
5.  50×TAE缓冲液
6.  阴性对照模板DNA
7.  正向引物(F,20μmol/L)及反向引物(R,20μmol/L)溶于灭菌$ddH_2O$中
8.  EB（溴化乙锭）或荧光染料（Gel-Red）

### 仪器

-   PCR管(0.2mL)
-   PCR仪(Biolab,带热盖)

## 实验操作

1.  引物设计
    1.  primer：一小段与目的基因共享同源性的单链DNA
    2.  fa
2.  引物配置
    1.  分子量计算公式：$$分子量 =C\times288+A\times312+G\times328+T\times303-61 $$或者==$$近似分子量\approx 碱基数\times 324.5$$==
    2.  引物工作浓度5-10 μmol/L,10 ×，20bp，2.0$OD_{260}$ $$1.0 OD_{260}=33\mu g Oligo DNA$$ 终浓度为50μmol/L
    3.  计算：$$分子量=20\times 324.5=6490 g/mol$$ $$质量数=2\times33=66\mu g$$ $$物质的量=66{\div}6490=0.010μmol$$ $$终体积=0.010{\div}50×10^6=200μL$$
    4.  离心机，$ddH_2O$溶解Oligo
3.  PCR反应组分
    1.  常规50μL反应体系：
        -   模板DNA1\~500ng
        -   正向引物5\~10ng
        -   反向引物5\~10ng
        -   1×PCR缓冲液
        -   $MgCl_2$
        -   dNTPS混合液
        -   *Taq*酶 1unit
    2.  以自制质粒DNA为模板的PCR反应：
        -   0.2mL PCR 管依次加入

            1.  10×PCR扩增缓冲液（含$MgCl_2$) 5μL
            2.  20mmol/L dNTPs混合液 1μL
            3.  5μmol/L正向引物 2.5μL
            4.  5μmol/L反向引物 2.5μL
            5.  $ddH_2O$ 37μL
            6.  模板质粒DNA 1μL
            7.  1\~5U/μL Taq聚合酶 1U

            -   总体积 50μL
4.  设置对照组实验
    1.  阳性对照：靶DNA片段
    2.  阴性对照：P154 正向/反向引物，有/无模板DNA，非特异性配对，污染
5.  PCR仪
    1.  Denaturation 高温95℃变性
    2.  Annealing 低温50\~65℃，55℃退火
    3.  Synthesis 中温72℃延伸
    4.  循环25——30次
6.  产物鉴定
    1.  DNA凝胶电泳
    2.  EB染色

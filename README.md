# lavaanMediation

[![R-CMD-check](https://github.com/hecongqing/lavaanMediation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hecongqing/lavaanMediation/actions/workflows/R-CMD-check.yaml)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
[![R >= 4.1](https://img.shields.io/badge/R-%3E%3D%204.1-276DC3.svg)](https://www.r-project.org/)

`lavaanMediation` 是一个基于 [Shiny](https://shiny.posit.co/) 和
[`lavaan`](https://lavaan.ugent.be/) 的交互式并行中介分析工具。它面向需要快速配置、检查并导出中介模型结果的研究者，同时保留完整的 `lavaan` 模型语法供复核。

> 当前版本为 `0.2.0`。应用用于辅助模型设定和结果整理，不能替代研究设计、模型识别、因果假设与统计诊断。

## 功能

- B1：`X → M → Y`，包含 `X → Y` 直接效应。
- C2：在 B1 基础上加入一个或多个协变量 `Z`，并估计 `Z → M` 与 `Z → Y`。
- 支持零个、单个或多个并行中介变量。
- 手动 Bootstrap 间接效应置信区间，并报告成功重采样次数。
- 展示模型语法、拟合指标、参数估计、R² 与路径图。
- 将 Model Fit 和 Parameter Estimates（含主效应模型、中介模型及 R²）导出为 CSV。
- 可上传 CSV，也可直接使用内置合成示例数据。

## 安装

```r
install.packages("remotes")
remotes::install_github("hecongqing/lavaanMediation")
```

## 运行

安装后在 R 中运行：

```r
lavaanMediation::run_app()
```

从源码运行：

```bash
git clone https://github.com/hecongqing/lavaanMediation.git
cd lavaanMediation
Rscript start_app.R
```

启动脚本不会自动修改你的 R 环境；如果缺少依赖，它会列出需要安装的包。

## 数据要求

CSV 第一行必须是变量名。进入模型的 X、M、Y、Z 必须：

- 是数值列；
- 在同一模型中互不重复；
- 具有可供 `lavaan` 估计的有效样本；
- 使用清晰且唯一的列名。

仓库在 [`inst/extdata`](inst/extdata) 中提供三份合成示例数据：

- `mediation_demo.csv`：单中介示例；
- `parallel_mediation_demo.csv`：多中介、多协变量示例；
- `anonymized_mediation_demo.csv`：使用通用变量名的同类示例。

## 模型语法

单中介 B1：

```text
M ~ a1*X
Y ~ cprime*X
Y ~ b1*M
indirect := a1*b1
total := cprime + (a1*b1)
```

包含协变量的 C2：

```text
M ~ a1*X
M ~ zm1_1*Z
Y ~ cprime*X
Y ~ b1*M
Y ~ zy1*Z
indirect := a1*b1
total := cprime + (a1*b1)
```

多中介模型会为每个中介生成独立的 `a`、`b` 和 `indirect` 标签，并计算总间接效应。

## 结果说明

- **Model Fit**：估计量、参数数、样本数、Bootstrap 次数、χ²、df、CFI、TLI、AIC、BIC、调整 BIC、RMSEA（90% CI）和 SRMR。
- **Parameter Estimates**：主效应模型与中介模型的非标准化/标准化系数、标准误、z、p 和 95% CI。
- **R²**：完整中介模型的 R²，以及用于效应保留比较的主效应模型 Y 的 R²。
- **Path Diagram**：分别绘制主效应模型与中介模型，显示标准化系数。
- **Download**：生成与当前分析配置一致的 CSV 文件。

## 项目结构

```text
lavaanMediation/
├── R/                         # 对外包接口（run_app）
├── inst/
│   ├── app/
│   │   ├── app.R             # 最小 Shiny 启动文件
│   │   ├── R/                # 模型、结果、绘图、UI、Server
│   │   └── www/              # 概念模型图片
│   └── extdata/              # 合成示例数据
├── tests/testthat/            # 模型、下载与 UI 测试
├── .github/workflows/         # R CMD check
├── start_app.R                # 源码启动入口
└── deploy_app.R               # shinyapps.io 部署入口
```

大型单文件应用已按职责拆分。结构参考了 [R Packages](https://r-pkgs.org/structure.html) 的包规范、
[Posit 的包内 Shiny 应用建议](https://shiny.posit.co/r/articles/share/deployment-local)、
[`golem`](https://github.com/ThinkR-open/golem) 的生产级职责分离思路，以及
[`r-lib/actions`](https://github.com/r-lib/actions) 的 R 包持续集成方案。

## 开发与检查

```r
install.packages(c("devtools", "testthat"))
devtools::test()
devtools::check()
```

提交问题前请阅读 [CONTRIBUTING.md](CONTRIBUTING.md)。

## 许可证

[MIT License](LICENSE.md) © 2026 Congqing He

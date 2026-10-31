# 深化研究方案 v2：竞争死亡主线（Early-Onset CRC Survival Paradox）

- 制定日期：2026-10-29
- 方案定位：在现有 SEER 早发/晚发结直肠癌研究基础上，把论文主线从“早发有生存优势”重构为“早发转移表型更重、但生存优势是暂时且主要由竞争死亡解释”，并用 TCGA/ICGC 做方向性分子印证。
- 目标期刊层级：影响因子约 6–9；版面费上限 3,000 美元（具体期刊在写作完成后按真实 APC 核实）。
- 本方案不替代旧版 `plan/analysis_plan.md`，旧版作为历史预注册保留；正式执行前由用户确认本方案后再更新主方案文件。

---

## 1. 研究问题与核心主张

**核心研究问题**：早发型结直肠癌（EOCRC，<50 岁）初诊转移更多、肿瘤生物学并不更温和，为什么总生存反而更好？这种优势在肿瘤特异死亡和绝对风险尺度上是否真实、持续？

**核心主张（一句话）**：EOCRC 具有更强的同步转移表型，其肿瘤内在程序并不比晚发型温和；所谓“早发生存优势”在肿瘤特异死亡上只是短暂存在，在绝对风险尺度上幅度有限，并主要由晚发型患者更高的非肿瘤竞争死亡所解释。

**四条假设**：
- H1（转移剂量反应）：年轻年龄与同步远处转移的关联是连续的、呈梯度关系，而不只是 50 岁切点两端的跳跃；器官特异模式在调整后仍存在。
- H2（优势短暂）：在 I–III 期根治性人群中，EOCRC 的肿瘤特异生存优势集中在诊断后第 1 年，随后衰减；5 年 RMST 与累积死亡绝对差异小于 HR 所暗示的幅度。
- H3（竞争死亡解释）：病因特异风险（csHR）与亚分布风险（sdHR）出现方向/幅度分离；绝对肿瘤死亡风险差异有限，而晚发型非肿瘤竞争死亡显著更高。
- H4（分子层面不更温和，探索性）：TCGA 早发肿瘤的增殖、间质/EMT、免疫、Wnt 等转录程序不低于晚发；ICGC 早发肿瘤的突变负荷/驱动基因改变不更低。该条只作方向性印证，不作确证性结论。

---

## 2. 研究设计与分析人群

### 2.1 数据来源
- **SEER 2000–2022**：8 个结直肠解剖亚部位，主分析队列沿用现有纳排（第一原发、腺癌、随访 >0、分期/部位可判定）。
- **SEER 2010–2022 现代子队列**：用于器官特异转移（肝、肺、骨、脑、多器官），因 SEER 器官转移字段自 2010 年起才有。
- **TCGA COAD + READ**：633 例临床（早发 77 例；有表达 70 例；初诊 M1 15 例；死亡 10 例），表达矩阵为 TPM log2，另含 11A 癌旁样本。
- **ICGC COCA-CN**：411 例供者（早发 97 例；321 例有体细胞突变，早发 83 例）；含 SNV、CNV 与临床随访。

### 2.2 主分析人群（本次方案的关键改动）
- **主生存分析：I–III 期（根治性人群）**。理由：生存悖论与竞争死亡的解释在根治性人群中才干净；IV 期本身预后极差、治疗与竞争死亡结构不同，混在一起会稀释机制。
- **次要分析：全分期（I–IV）**，用于与旧稿衔接及敏感性验证。
- **IV 期单独描述**：转移谱、竞争死亡构成、预后，不进入主生存模型。
- **转移谱分析**：2010–2022 全部分期（含 IV 期），因为结局就是初诊转移。
- **年龄缺失处理**：旧脚本把缺失年龄填为 65 岁；本方案改为**排除年龄缺失**并在流失表中记录，避免连续年龄分析被错误填充污染。

### 2.3 暴露与协变量
- 暴露：年龄连续变量（主分析，RCS 5 节点）与年龄组（<50 vs ≥50，用于临床可解释的对比）。
- 协变量：性别、种族/族裔、诊断年份、解剖亚部位（右半/左半/直肠）、组织学类型（经典型 vs 黏液/印戒）、分级、手术、化疗、放疗、检出淋巴结数；转移模型另加基线 CEA（2010+）。
- 分期：主生存模型限定 I–III 期；转移模型不调整总体分期（M 即结局的一部分），但可调整 T/N 作为敏感性分析（若 SEER 字段可稳定提取）。

### 2.4 权重
- 在**每个目标人群内重新估计**倾向评分与稳定化 IPTW（I–III 期人群、全分期人群分别估计），不沿用旧权重。
- 平衡标准：所有协变量绝对 SMD < 0.05；同时报告加权前后 SMD 图。
- 极端权重：1%–99% 截断作为敏感性分析；报告权重分布。

---

## 3. 分析模块与统计方法

### S1 队列构建与变量衍生（沿用 + 修正）
- 沿用现有 8 亚部位合并、第一原发、腺癌、随访 >0 等纳排。
- **修正**：年龄缺失不填 65；记录缺失例数；年龄上限按 SEER 90+ 处理（90 岁及以上合并）。
- 产出：`data/cleaned_cohort_full_v2.rds`、`data/cleaned_cohort_modern_v2.rds`、`results/tableS1_attrition_v2.csv`。

### S2 IPTW 平衡（分人群重估）
- 方法：稳定化 IPTW，多变量 logistic 倾向评分；`cobalt::bal.tab` 检查 SMD；权重截断敏感性。
- 产出：`results/table1_baseline_v2.csv`、`results/tableS2_iptw_balance_v2.csv`、`figures/fig1_consort_iptw_v2.png`。

### S3 器官特异转移谱 + 连续年龄剂量反应（新增核心）
- 主模型：多变量 logistic 回归，结局为任何远处转移、肝、肺、骨、脑、多器官（≥2）转移；暴露为年龄组（<50 vs ≥50），调整性别、种族、年份、亚部位、组织学、分级、CEA。
- **新增剂量反应**：年龄连续 RCS（5 节点）进入同一逻辑回归，参考年龄 50 岁；用 `rms::lrm` + `rms::rcs`，`anova` 检验非线性，`Predict` 输出 OR 曲线。
- 产出：`results/table2_metastasis_logistic_v2.csv`、`results/table2b_age_rcs_metastasis.csv`、`figures/fig2_metastasis_tropism_v2.png`（含 RCS 面板）。

### S4 生存动态（I–III 期为主）
- 主模型：IPTW 加权 Cox（`survival::coxph`，`weights=iptw_w`，`robust=TRUE`），并用协变量双重调整作为敏感性。
- 时变风险：沿用时间分段 Cox（0–12、12–36、36–60、60–120、>120 月），并新增连续年龄 RCS 的 Cox（`rms::cph`，`weights`，`robust=TRUE`）绘制年龄-HR 曲线。
- 比例风险检验：`cox.zph`；违反时以时间分段/RCS 为主，不强行报告单一 HR。
- 产出：`results/table3_survival_models_v2.csv`、`figures/fig3_survival_dynamics_v2.png`。

### S5 条件生存（沿用 + 分期分层）
- 5 年条件 OS/CCSS 在 0–5 年地标点计算；按 I–III 期主分析、IV 期单独；检查每个地标剩余风险集事件数（>50）。
- 产出：`results/table_conditional_survival_v2.csv`、`figures/fig4_conditional_survival_v2.png`。

### S6 竞争风险（升级为调整模型 + 绝对风险）
- CIF：`cmprsk::cuminc` 计算 CCSM 与 NCSM 累积发生率，Gray 检验；报告 5 年、10 年 CIF 及组间绝对差（含 95% CI）。
- 病因特异 Cox：`coxph(Surv(time, status==1) ~ EOCRC + covariates, weights=iptw_w, robust=TRUE)`；NCSM 同理。
- **调整 Fine-Gray**：`survival::finegray` 生成竞争风险数据，再以 `coxph` 拟合亚分布模型，权重为 IPTW × finegray 权重；与未调整 `cmprsk::crr` 对照。若 `finegray` 权重处理不稳定，退用 `riskRegression::FGR`（无权重，仅作敏感性），并在审计中说明。
- 解释口径：同时报告 csHR 与 sdHR，明确二者回答不同问题；**不以 sdHR 单独否定 csHR**，而以绝对 CIF 差异作为临床尺度。
- E-value：对 csHR、sdHR、转移 OR 分别计算（`EValue::evalues.HR`、`evalues.OR`）。
- 产出：`results/table4_competing_risks_v2.csv`、`results/table4b_cif_absolute_differences.csv`、`figures/fig5_competing_mortality_v2.png`。

### S7 绝对风险与 RMST（新增）
- 5 年与 10 年 RMST：`survival::survfit` 按组拟合（IPTW 加权），`print(..., rmean=tau)` 提取加权 RMST 与 SE；组间差及 95% CI 用 delta 法或 bootstrap（1,000 次）。
- 另用 `survRM2::rmst2` 做未加权、协变量调整的 ANCOVA 型 RMST 作为对照，确保方法可核对。
- 产出：`results/table5_rmst.csv`；并入 Figure 3 的绝对风险面板。

### S8 亚组与交互（沿用）
- 亚部位、分期（I–II vs III）、性别、诊断年代（2000–2009 vs 2010–2022）与年龄组的交互；报告 P for interaction；每个亚组最小事件数 ≥20。
- 产出：`results/tableS3_subgroup_interactions_v2.csv`。

### S9 敏感性分析与偏倚（沿用 + 扩展）
- 权重 1%–99% 截断；仅经典型腺癌；排除 DCO/尸检；年龄缺失排除；全分期 vs I–III 期；排除 2020–2022（疫情随访影响）可选。
- 多重比较：对分子模块与亚组交互采用 Benjamini-Hochberg FDR。
- 未测混杂：E-value；对转移 OR 与生存 HR 分别报告。
- 产出：`results/tableS4_sensitivity_v2.csv`、`scripts/analysis_audit_v2.md`。

### S10 TCGA 分子方向性印证（探索性）
- **M1 关联**：TCGA 早发 vs 晚发初诊 M1，logistic 回归调整性别、部位（结肠/直肠）、T、N；报告 OR 与 95% CI；明确标注样本量与把握度不足。
- **转录程序评分**：用预定义基因集计算增殖（MKI67、PCNA、CCNB1、CDK1、TOP2A、BUB1、AURKA）、间质/EMT（VIM、CDH2、FN1、ZEB1、ZEB2、SNAI1、SNAI2、TWIST1、MMP2、MMP9、COL1A1、COL1A2、FAP、ACTA2）、上皮（CDH1、EPCAM、KRT8/18/19/20、CLDN3/4、MUC1）、Wnt（AXIN2、LGR5、MYC、CCND1、ASCL2、RNF43、NKD1、SP5、LEF1）、免疫（CD8A、GZMA、PRF1、CD274、CXCL9、CXCL10、IFNG）。
- 每个样本取基因 z 分数均值；线性模型校正分期、部位、性别；报告早发 vs 晚发的标准化差与 FDR。
- **不纳入**：CMS 分型、免疫解卷积、突变签名（本次范围定为“方向性印证+探索”，避免早发样本不足导致的过度解释）。
- 产出：`results/table6_tcga_molecular.csv`、`figures/fig6_molecular_triangulation_v2.png`（A/B 面板）。

### S11 ICGC 基因组方向性印证（探索性）
- **突变负荷**：按供者汇总体细胞突变数；比较早发 vs 晚发（log10 转换、线性模型/ Wilcoxon）；报告超突变（>5,000 突变）比例与 Fisher 检验；排除超突变者做敏感性。
- **驱动基因**：将 Ensembl ID 映射为 APC、TP53、KRAS、SMAD4、BRAF、PIK3CA、ARID1A；比较早发 vs 晚发突变频率（Fisher），FDR 校正。
- **M1 比例**：报告早发 vs 晚发初诊 M1，但明确 ICGC 分期编码与手术队列选择可能造成与 SEER 方向不一致；不把 ICGC 当作转移谱的复制队列。
- 产出：`results/table7_icgc_genomic.csv`、`figures/fig6_molecular_triangulation_v2.png`（C/D 面板）。

### S12 整合与结论台账
- 将 SEER 表型、TCGA 转录、ICGC 基因组三层证据整理为“一致/不一致/不确定”矩阵；只在方向一致处下结论，不一致处写入局限。
- 产出：`results/claims_v2.md`（结论台账）、`plan/分析调整说明_v2.md`（偏离记录）。

---

## 4. 图表蓝图（按面板规划）

### 主图
- **Figure 1**（2 面板）：(A) 队列纳排流程图；(B) IPTW 前后 SMD Love plot。主面板 B。
- **Figure 2**（3 面板）：(A) 早发 vs 晚发各器官转移率；(B) 多变量 aOR 森林图（主面板）；(C) 连续年龄 RCS 的 OR 曲线（任何/肝/肺转移）。
- **Figure 3**（4 面板）：(A) IPTW 加权 OS 曲线；(B) IPTW 加权 CSS 曲线；(C) 时间分段时变 HR；(D) RMST/绝对风险差森林图（主面板之一）。
- **Figure 4**（3 面板）：(A) 5 年条件 OS；(B) 5 年条件 CCSS；(C) 分期分层条件生存热图。
- **Figure 5**（4 面板）：(A) CCSM 累积发生率；(B) NCSM 累积发生率；(C) 死因构成随时间变化；(D) 调整后 csHR vs sdHR 森林图（主面板）。
- **Figure 6**（4 面板）：(A) TCGA 早发 vs 晚发 M1 比例与 OR；(B) TCGA 转录程序标准化差森林图；(C) ICGC 突变负荷/超突变比例；(D) ICGC 驱动基因频率差。

### 主表
- **Table 1**：I–III 期主分析人群基线特征（加权前后）。
- **Table 2**：转移谱 logistic 模型 + 年龄 RCS 非线性检验。
- **Table 3**：生存模型（未加权 Cox、IPTW Cox、时间分段、RMST）。
- **Table 4**：竞争风险（CIF、csHR、sdHR、绝对差、E-value）。
- **Table 5**：TCGA 与 ICGC 分子/基因组结果汇总。

### 补充材料（`manuscript/supplementary/`）
- Table S1 流失表；S2 平衡表；S3 亚组交互；S4 敏感性与 E-value；S5 分子敏感性；S6 模型诊断；S7 IV 期单独分析；S8 全分期敏感性。
- Figure S1 倾向评分密度；S2 Schoenfeld 残差；S3 亚部位分层生存；S4 IV 期转移谱；S5 分子评分敏感性。

---

## 5. 方法学实现表（新增与升级步骤）

| 步骤 | 必做 | 包（版本） | 函数 | 关键参数与依据 | 前提检验 | 前提不成立时的备选 | 产出 |
|---|---|---|---|---|---|---|---|
| S9 年龄 RCS 转移模型 | 是 | rms (>=6.8) | rms::lrm, rms::rcs, anova, Predict | 5 节点；参考年龄 50；调整性别/种族/年份/亚部位/组织学/分级/CEA | 非线性 anova P；VIF<5；事件数/变量≥10 | 改为年龄三分位/分段线性；或仅报告年龄组 | fig2c, table2b |
| S10 年龄 RCS 生存模型 | 是 | rms, survival | rms::cph, Predict | 5 节点；weights=iptw_w；robust=TRUE；I–III 期主分析 | cox.zph；事件数/变量≥10 | 时间分段 Cox；或年龄组 | fig3, table3 |
| S11 RMST | 是 | survival, survRM2 | survfit + print(rmean), rmst2 | tau=60/120 月；加权 RMST + bootstrap 1,000 次；rmst2 作未加权对照 | 各组 tau 内风险集人数>50 | 仅报告 5 年 RMST；或改用 CIF 绝对差 | table5, fig3d |
| S12 调整 Fine-Gray | 是 | survival, cmprsk, riskRegression | finegray + coxph; crr; FGR | 竞争事件=2；IPTW×finegray 权重；事件编码 0/1/2 | 竞争事件数≥20；权重分布检查 | 仅未调整 crr + 敏感性；或 cause-specific 为主 | fig5, table4 |
| S13 E-value | 是 | EValue | evalues.HR, evalues.OR | rare 按 5 年发生率判断；HR/OR 及 CI 下限 | 无 | 手动公式核对一个例子 | tableS4 |
| S14 TCGA 分子 | 是 | data.table, stats | glm, lm, p.adjust | 基因 z 分数均值；校正分期/部位/性别；FDR | 基因在矩阵中存在；样本匹配率>90% | 改用秩和检验；或仅报告描述性 | table6, fig6a-b |
| S15 ICGC 基因组 | 是 | data.table, stats | glm, fisher.test, p.adjust | log10 突变数；超突变>5000；驱动基因 Fisher；FDR | Ensembl 映射成功率；突变数据覆盖 321 例 | 仅报告突变负荷；驱动基因改为描述性 | table7, fig6c-d |
| S16 敏感性与多重比较 | 是 | stats | p.adjust, glm, coxph | BH-FDR；权重截断；分期/年代限制 | 各敏感性模型事件数 | 减少分层；合并亚组 | tableS4, tableS5 |

---

## 6. 目标期刊与预算（初定，写作后核实）

- 目标层级：影响因子约 6–9；优先可走订阅通道或 APC ≤3,000 美元的混合刊。
- 备选方向（待 JournalDB 与官网核实）：British Journal of Cancer、Cancer Letters、European Journal of Cancer、International Journal of Cancer、Journal of Translational Medicine、BMC Medicine、Clinical and Translational Medicine。
- 选刊阶段必须重新用 PubMedSearch 统计同类文章去向，并用 JournalDB 核实分区/IF；不得沿用旧推荐。

---

## 7. 主要风险与限制（写作时必须如实披露）

1. **SEER 无复发/无病生存**：只能用条件生存与肿瘤特异死亡替代，不能声称无复发生存。
2. **TCGA 早发样本少**：70 例有表达、15 例 M1、10 例死亡；分子部分只能做方向性印证，不能做早发专属生存验证。
3. **ICGC 分期编码与选择偏倚**：M1 比例与 SEER/TCGA 方向不一致，不能作为转移谱的干净复制；必须如实报告并解释。
4. **竞争风险解释依赖假设**：csHR 与 sdHR 回答不同问题；以绝对 CIF/RMST 作为临床尺度，避免“优势被完全抹平”的绝对化表述。
5. **残余混杂**：SEER 缺具体化疗方案、靶向治疗、分子分型；IPTW 只能平衡已测变量。
6. **年龄切点**：以连续年龄 RCS 为主，50 岁切点仅作临床可解释的对比。
7. **无功能实验**：分子部分为生物信息学关联，不能下机制因果结论。

---

## 8. 执行顺序

1. 用户确认本方案。
2. S1–S2：重建队列与分人群 IPTW。
3. S3–S7：SEER 主分析（转移、生存、条件生存、竞争风险、RMST）。
4. S8–S9：亚组与敏感性。
5. S10–S11：TCGA/ICGC 分子与基因组探索。
6. S12：整合、结论台账、图表定稿。
7. 进入论文重写与选刊。

---

## 9. 参考范文（已核实的真实 PMID）

- 42600241：ESMO Open，I–III 期乳腺癌时变风险、CIF、dRMST、地标分析；本方案 S4–S7 的主要范例。
- 42521664：Nature Communications，SEER 竞争死因分解与 Fine-Gray；本方案 S6 的主要范例。
- 42842843：JCO Precision Oncology，SEER IPTW/PSM 与稳健性；本方案 S2 的范例。
- 42458797：JNCI，早发癌流行病学趋势；引言与讨论的流行病学依据。
- 30854646：Cancer，早发 CRC 临床与 CMS 特征；分子讨论依据。
- 36921601：Cell Reports Medicine，早发 CRC 多组学；分子模块的对照与差异化依据。
- 40301858：BMC Medicine，TCGA 早发 CRC 多组学；分子模块的对照与差异化依据。
- 33219329：Nature Reviews Clinical Oncology，早发 CRC 综述；背景依据。
- 38050068、40188208、40553883：CRC 器官特异转移/器官趋向性的单细胞、细胞遗传学与部分 EMT 证据；Figure 6 讨论依据。

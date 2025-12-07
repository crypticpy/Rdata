# Cross-Pathogen Detective Analysis: H3N2 Reports vs. COVID-19, RSV, and Other Respiratory Pathogens

## Executive Summary

As of early December 2025, influenza A(H3N2) is the dominant subtype in many regions globally and in the United States, coinciding with a period of low but rising COVID-19 activity and seasonal increases in respiratory syncytial virus (RSV). This cross-pathogen analysis finds that most reported H3N2 signals are consistent with contemporaneous influenza surveillance. However, four anomalies warrant heightened scrutiny and targeted laboratory follow-up: the first U.S. death attributed to influenza A(H5N5) amid seasonal H3N2 circulation; potential antigenic drift concerns associated with H3N2 subclade K and its divergence from vaccine reference strains; an apparent surveillance reporting gap around late September 2025 that temporarily limited visibility into U.S. trends; and laboratory testing limitations that can produce false negatives and false positives, with downstream effects on case classification, case-mix interpretation, and public health messaging.

Globally, the World Health Organization (WHO) reports that in week 47 (ending November 23, 2025) influenza activity continued to increase and that influenza A(H3N2) predominated across large swaths of Europe, Asia, the Americas, and Africa, with co-dominance with A(H1N1)pdm09 noted in Central America and the Caribbean[^1]. In the United States, the Centers for Disease Control and Prevention (CDC) indicates that seasonal influenza remains low but is increasing, with H3N2 the predominant subtype in public health laboratory detections and hospitalizations; at the same time, RSV activity is rising in several regions and COVID-19 activity is very low nationally in clinical and wastewater indicators[^3][^8]. WHO’s SARS-CoV-2 variant tracking shows that JN.1 and its descendants continue to be monitored closely, with other variants under monitoring (e.g., KP.3.1.1, LP.8.1) reflecting ongoing evolution but not a clear surge coincident with current U.S. respiratory illness trends[^9]. 

Several signals align with expected seasonal patterns: rising influenza positivity and hospitalizations in the U.S., H3N2 dominance in both U.S. and many international settings, and RSV increases concentrated among young children in the southeastern, southern, and mid-Atlantic regions. Yet, there are features that can confound straightforward interpretation: ILINet (outpatient syndromic surveillance) captures influenza-like illness (ILI) attributable to multiple pathogens, which can complicate attribution when non-influenza respiratory viruses circulate; antigen and molecular tests differ in sensitivity and specificity and can yield false results, particularly at low viral loads; and clade-level changes in H3N2 (e.g., subclade K) introduce uncertainty around antigenic match and clinical severity expectations[^8][^11][^12][^13][^14][^18].

Actionably, public health authorities should prioritize confirmatory PCR testing and sequencing for any unusual or inconsistent respiratory presentations, expand targeted surveillance where ILI rises without corresponding influenza positivity, implement multi-target molecular assays to reduce misclassification risk, and strengthen communications that contextualize H3N2 signals within broader respiratory virus activity—especially when outpatient ILI may be influenced by co-circulating pathogens. Laboratory networks should monitor for assay drift in H3N2 targets and ensure quality controls and external proficiency testing to mitigate false negatives and false positives[^8][^15].

## Methodology and Data Sources

This analysis triangulates multiple authoritative surveillance systems to assess whether reported H3N2 activity is consistent with influenza surveillance and with the co-circulation of other respiratory pathogens. Core sources include the CDC FluView weekly reports, the CDC Respiratory Illnesses Data Channel, WHO influenza updates and SARS-CoV-2 variant tracking, ECDC influenza and variant threat assessments, public health laboratory and hospitalization surveillance datasets, and peer-reviewed studies on diagnostic performance and co-infections[^8][^3][^1][^9][^2][^6][^7][^14][^12][^13][^19][^20].

We apply structured anomaly detection criteria:
- Temporal anomalies: H3N2 detections outside expected seasonality, or ILI spikes without accompanying influenza positivity.
- Geographic inconsistencies: Regions reporting H3N2 but lacking typical seasonal context or showing discordant viral distribution.
- Severity mismatches: Hospitalization or mortality patterns that deviate from known H3N2 clade behavior or expected age-specific risk profiles.
- Laboratory anomalies: Results inconsistent with test performance characteristics, including discordant rapid vs. PCR findings, low positivity with high ILI, or repeated indeterminate tests.

To support transparency, we summarize the source coverage in Table 1.

To illustrate the scope and granularity of data used, the following table summarizes major data sources, the pathogen(s) they cover, geography, recency, and metrics available.

Table 1. Data Source Inventory and Coverage

| Source | Pathogen(s) Covered | Geography | Reporting Cadence | Last Update Used in Analysis | Key Metrics Available |
|---|---|---|---|---|---|
| CDC FluView Weekly Reports (Week 47)[^8] | Influenza (A/B, subtypes), hospitalizations, mortality indicators | United States | Weekly | Week 47 (ending Nov 22, 2025) | Clinical/public health lab positivity, virus distribution, ILINet, ED visits, hospitalizations, genetic characterization |
| CDC Respiratory Illnesses Data Channel[^3] | Influenza, COVID-19, RSV, Mycoplasma, Pertussis | United States | Seasonal updates | Dec 5, 2025 | Activity levels, ED trends, wastewater indicators, regional patterns |
| WHO Influenza Updates[^1] | Influenza (global) | Global | Weekly | Week 47 (ending Nov 23, 2025) | Global positivity, regional subtype distribution |
| WHO Variant Tracking[^9] | SARS-CoV-2 variants | Global | Periodic | Dec 5, 2025 | VOI/VUM designations, risk evaluations |
| ECDC Influenza Virus Characteristics[^6][^7] | Influenza (EU/EEA) | EU/EEA | Periodic | 2024–2025 and 2025 summaries | Genetic/antigenic characterization, clade distribution |
| CDC MMWR Genomic Surveillance[^5] | SARS-CoV-2 variants | United States | Periodic | 2024 | Vaccine antigen selection, surveillance approach |
| WHO Season Summary (Oct 2024–May 2025)[^4] | Influenza (global) | Global | Seasonal summary | Sep 18, 2025 | Season context and regional trends |
| Clinical and Laboratory Standards (see references)[^11][^12][^13][^14] | Diagnostics | Multi-regional | Study periods | 2023–2025 | Sensitivity/specificity, false positive/negative risks |
| Co-infection studies[^19][^20] | SARS-CoV-2, influenza, RSV | Multi-regional | 2023–2024 | 2024–2025 | Co-detection frequencies, clinical interpretation |

Limitations include data lags (e.g., mortality reporting), syndromic overlap (ILI capturing multiple pathogens), and incomplete state-level detail across all pathogens. These caveats are incorporated into the interpretation and recommendations.

## Current Cross-Pathogen Activity (as of early December 2025)

Influenza A(H3N2) is the leading subtype in many regions, and U.S. indicators show low but increasing seasonal influenza activity. In parallel, RSV activity is rising, especially in children aged 0–4 years in parts of the Southeast, South, and Mid-Atlantic, while COVID-19 activity remains low in clinical settings and wastewater. WHO reports that influenza A(H3N2) predominates across Europe, Asia, the Americas, and Africa, with co-dominance with A(H1N1)pdm09 in Central America and the Caribbean[^1]. The CDC’s integrated view aligns with this pattern, noting increasing influenza activity with H3N2 predominance and gradual RSV increases, while COVID-19 remains very low nationally[^3][^8].

To make this juxtaposition explicit, Table 2 compares national activity levels and trend directions across the three major respiratory viruses.

Table 2. US National Activity Snapshot (CDC Data Channel; Week 47 context)

| Pathogen | Activity Level | Trend | Notes |
|---|---|---|---|
| Influenza (H3N2 predominant)[^8][^3] | Low | Increasing | Rising positivity in clinical labs; increasing hospitalizations, highest in older adults |
| RSV[^3] | Low | Increasing | Regional increases in Southeast, South, Mid-Atlantic; ED visits/hospitalizations up in children 0–4 |
| COVID-19[^3][^9] | Very Low | Increasing (ED visits) | Low clinical activity; wastewater activity very low nationally; variant evolution ongoing |

CDC’s Respiratory Virus Activity Levels mirror this low-to-rising profile for influenza and RSV, with COVID-19 at very low levels nationally[^16]. This alignment supports a coherent seasonal picture: an uptick in influenza and RSV, while COVID-19 remains subdued, although viral evolution continues as reflected in WHO’s variant tracking[^9].

## Cross-Reference: H3N2 Reports vs. Influenza Surveillance

At the national level, reported H3N2 signals correspond to influenza surveillance trends: positivity is rising but from low baselines; public health laboratories show a strong A(H3N2) signal; and hospitalized cases are predominantly influenza A, with A(H3N2) accounting for approximately three-quarters of subtyped hospitalizations since October 1, 2025[^8]. These patterns are consistent with WHO’s global assessment of H3N2 predominance[^1].

To illustrate, Table 3 summarizes the U.S. public health laboratory virus distribution for week 47.

Table 3. US Public Health Labs (Week 47): Virus Distribution and Subtyping[^8]

| Category | Count | Percentage |
|---|---:|---:|
| Total positive specimens | 358 | — |
| Influenza A (all) | 343 | 95.8% |
| Influenza B (all) | 15 | 4.2% |
| Influenza A subtyped (of A) | 305 | 88.9% |
| H1N1pdm09 (of subtyped A) | 54 | 17.7% |
| H3N2 (of subtyped A) | 251 | 82.3% |
| H5 (of subtyped A) | 0 | 0% |
| Cumulative since Week 40: A(H3N2) (of subtyped A) | 1610 | 74.6% |
| Cumulative since Week 40: H1N1pdm09 (of subtyped A) | 547 | 25.3% |

Genetic characterization data further contextualize the subtype distribution. Table 4 details U.S. influenza virus characterization since May 18, 2025, including A(H3) subclades and the growing share attributed to subclade K.

Table 4. Influenza Genetic Characterization (US; since May 18, 2025)[^8]

| Clade/Subclade | Count | Percentage |
|---|---:|---:|
| A(H1) tested | 305 | — |
| A(H1) HA clade 5a.2a.1 | 297 | 97.4% |
| A(H1) HA subclade D.3.1 | 295 | 96.7% |
| A(H3) tested | 160 | — |
| A(H3) HA clade 2a.3a.1 | 160 | 100% |
| A(H3) subclade K | 91 | 56.9% |
| A(H3) other J.2.x subclades | 69 | 43.1% |
| B(Victoria) tested | 124 | — |
| B(Victoria) HA clade 3a.2 | 124 | 100% |
| B(Yamagata) tested | 0 | — |

Cumulatively, A(H3N2) represents roughly three-quarters of subtyped influenza A detections since the start of the season (week 40), and within A(H3), subclade K has become the majority since late September 2025. This alignment between U.S. and global signals suggests that reported H3N2 activity reflects true influenza dynamics rather than systematic misclassification. It also underscores the importance of clade-level monitoring given antigenic implications.

## Cross-Reference: H3N2 Reports vs. COVID-19 Variants

WHO’s variant tracking shows that JN.1 remains a variant of interest, while KP.3.1.1, LP.8.1, NB.1.8.1, XFG, and BA.3.2 are under monitoring[^9]. ECDC’s variant assessments for late 2025 similarly reflect de-escalation of earlier Omicron lineages from concern status, with current monitoring focused on newer descendants[^10]. MMWR reporting highlights how national genomic surveillance has guided vaccine antigen selection (e.g., XBB.1.5 and JN.1) over recent seasons[^5]. 

Against this variant backdrop, U.S. COVID-19 clinical activity is very low, with very low wastewater signals, and only modest increases in emergency department visits—suggesting that co-circulation is occurring at low levels and is unlikely to explain rising ILI trends in the absence of elevated COVID-specific indicators[^3]. In other words, H3N2 increases appear to be primarily influenza-driven, not secondary to a concurrent surge in SARS-CoV-2.

Table 5 situates key SARS-CoV-2 variants and their public health status.

Table 5. SARS-CoV-2 Variants of Interest/Under Monitoring (as of Dec 2025)[^9][^10]

| Designation | Pango Lineage | Clade | Notable Features | Status |
|---|---|---|---|---|
| VOI | JN.1 | 24A | BA.2.86 + S:L455S | Variant of Interest |
| VUM | KP.3.1.1 | 24E | KP.3 + S:S31- | Variant under Monitoring |
| VUM | LP.8.1 | 25A | Multiple spike changes (e.g., S:F456L, S:Q493E) | Variant under Monitoring |
| VUM | NB.1.8.1 | 25B | JN.1 + S:T22N, S:F59S, S:F456L | Variant under Monitoring |
| VUM | XFG | 25C | JN.1 + S:T22N, S:S31P, S:R346T, S:F456L | Variant under Monitoring |
| VUM | BA.3.2 | — | Extensive mutation profile | Variant under Monitoring |

## Cross-Reference: H3N2 Reports vs. RSV and Other Respiratory Pathogens

RSV activity is increasing in the U.S., with clear signals among children aged 0–4 years in several HHS regions (southeastern, southern, mid-Atlantic). Influenza activity is also increasing but remains low overall, with a different age profile and geographic pattern. COVID-19 remains low, consistent with the absence of variant-driven surges in clinical indicators[^3]. The net effect is a mixed respiratory season with rising RSV and influenza against a low COVID baseline.

Beyond these three viruses, the CDC notes that Mycoplasma pneumoniae is elevated in some U.S. areas and that pertussis remains elevated compared to pre-pandemic levels, albeit below the November 2024 peak[^3]. These additional pathogens contribute to the respiratory illness backdrop but do not materially alter the core finding: current H3N2 signals align with influenza seasonality, while RSV increases are occurring where expected, and COVID-19 is not currently driving trends.

## Suspicious Patterns and Potential Anomalies

Several findings stand out:

1) Novel influenza A(H5N5) death amid seasonal H3N2 circulation. The first U.S. death with influenza A(H5N5) was reported in mid-November 2025. Importantly, there is no evidence of human-to-human transmission of influenza A(H5) viruses in the U.S., and the case appears biologically distinct from seasonal H3N2 dynamics[^8]. The salient anomaly is the juxtaposition of a rare avian-linked fatality with ongoing seasonal H3N2 predominance.

2) Subclade K mismatch and vaccine effectiveness signals. H3N2 subclade K has diverged antigenically from the current vaccine reference strain, with early data suggesting reduced VE against medical attendance, especially in adults, though not to zero effectiveness[^18]. EU/EEA risk assessments similarly note significant divergence of subclade K from northern hemisphere vaccine strains[^18]. This introduces a plausible anomaly: if H3N2 subclade K were to cause disproportionate illness despite vaccination, severity patterns could appear inconsistent with historical H3N2 seasons unless adjusted for VE and age-specific immunity.

3) Surveillance reporting gap. Experts have highlighted uncertainty due to an apparent absence of standard weekly CDC respiratory illness surveillance data around late September 2025[^18]. While reporting has since resumed, such gaps can limit situational awareness, complicate trend continuity, and affect real-time risk communication.

4) Diagnostic limitations potentially causing misclassification. Multiplex lateral flow assays show variable sensitivity and specificity for influenza A, with performance strongly dependent on viral load and substantial false-negative risk at higher CT values. Some tests underperform WHO minimum criteria for specificity, raising false-positive risks and the possibility of influenza attribution when non-influenza pathogens are responsible[^11]. Assay designs that rely on single targets are more susceptible to false negatives due to genetic drift in targets like the influenza matrix gene[^12]. 

To make these diagnostic limitations concrete, Table 6 summarizes performance metrics for three multiplex lateral flow tests.

Table 6. Multiplex Lateral Flow Test Performance for Influenza A[^11]

| Test | Sensitivity (95% CI) | Specificity (95% CI) | Notes |
|---|---|---|---|
| Microprofit | 82.1% (77.0–86.5%) | 97.6% (95.9–98.7%) | Performance drops at low viral loads; WHO minimum criteria met |
| Goldsite | 84.9% (80.9–88.3%) | 98.0% (96.2–99.1%) | Similar load dependence; largely meets WHO criteria |
| SureScreen | 89.7% (87.0–91.9%) | 86.2% (83.9–88.3%) | Specificity below WHO recommendations; higher false-positive risk |

5) ILI vs. influenza positivitydiscordance. ILINet captures respiratory illness from multiple pathogens, not just influenza. During periods of rising RSV or other non-influenza circulation, ILI may increase without corresponding rises in influenza positivity. This can look like an anomaly if interpreted solely as influenza activity[^8].

Table 7 provides a concise anomaly matrix with plausible explanations and follow-up actions.

Table 7. Anomaly Matrix

| Signal/Observation | Expected Pattern | What Was Observed | Plausible Explanation | Follow-up Actions |
|---|---|---|---|---|
| H5N5 death in context of seasonal H3N2 | Sporadic avian-linked cases, no human-to-human spread | First U.S. H5N5 death amid H3N2 predominance | Distinct zoonotic event; no change in H3N2 transmission | Sequence case virus; verify source exposure; enhanced novel influenza surveillance |
| Subclade K VE mismatch signals | VE varies by season/strain; some drift expected | Reduced VE against medical attendance in adults (early signals) | Antigenic drift vs. vaccine strain | Monitor antigenic characterization; adjust communications; encourage high-risk vaccination |
| Late Sep 2025 surveillance gap | Continuous weekly reporting | Temporary absence of standard weekly summaries | Operational/reporting interruption | Confirm resumption; document continuity; communicate transparently |
| LFI false positives/negatives | High performance at high load; lower at low load | Variability in specificity/sensitivity across brands | Viral load dependence; assay design limits | Prioritize PCR for discordant results; use multi-target assays |
| ILI rises without influenza positivity | Mixed pathogen circulation | ILI may rise due to RSV/Mycoplasma/pertussis | Syndromic overlap | Increase targeted testing; parse ILI by pathogen-specific data |

## Laboratory Testing Patterns and Capabilities

The national testing ecosystem relies on a mix of rapid antigen assays, multiplex lateral flow tests (LFI), and laboratory-based reverse transcription polymerase chain reaction (RT-PCR), with increasing use of multiplex panels that detect influenza A/B, RSV, and SARS-CoV-2. Each modality carries distinct performance characteristics:

- Antigen and lateral flow tests offer speed and accessibility but are less sensitive than PCR, particularly at low viral loads. In ambulatory settings, their sensitivity for influenza A can fall below WHO minimum criteria for some tests, and false negatives increase sharply when CT values are ≥30[^11].
- Multiplex PCR assays provide higher sensitivity and specificity but take longer to return results and may be less available in point-of-care contexts. They remain the reference standard for confirmatory testing and for differentiating co-infections[^14].
- Test design matters. Single-target designs are more vulnerable to false negatives when genetic drift affects primer/probe binding sites (e.g., matrix gene mutations in influenza A), whereas multi-target designs can mitigate this risk[^12].
- Emerging technologies (e.g., CRISPR-based detection) show promise but can encounter challenges in multiplex settings, including false negatives in low-concentration samples and discordance between targets, highlighting the need for robust validation and quality control[^13].

Table 8 consolidates the diagnostic modality matrix.

Table 8. Diagnostic Modality Matrix

| Modality | Typical Targets | Sensitivity/Specificity (Key Findings) | Failure Modes | Recommended Use Cases |
|---|---|---|---|---|
| Rapid Antigen | Influenza A/B; SARS-CoV-2; RSV | Lower sensitivity vs. PCR; brand-specific variability | False negatives at low viral loads; suboptimal specificity for some brands | Initial triage; outbreak settings; when PCR unavailable |
| Multiplex Lateral Flow (LFI) | Influenza A/B; SARS-CoV-2; RSV (±) | Sensitivity 82–90% for Influenza A; specificity as low as ~86% for one brand | False positives (specificity gaps); load-dependent sensitivity | Ambulatory screening; resource-limited settings |
| Multiplex PCR | Influenza A/B; RSV; SARS-CoV-2 | High sensitivity/specificity; reference standard | Longer turnaround; resource-intensive | Confirmatory testing; co-infection workup; hospital admission |
| CRISPR-based Multiplex | RNA targets without amplification (varies) | Sensitivity reduced at low concentrations; discordance between targets | Nonspecific cleavage; low sample concentration; degradation | Research/validation; specialized clinical contexts |
| Single-target Molecular Assays | One gene per pathogen | Higher risk of false negatives with drift | Primer/probe mismatch | Use with caution for evolving viruses |
| Multi-target Molecular Assays | ≥2 genes per pathogen | Reduced false-negative risk; broad strain coverage | Complexity; cost | Preferred for influenza and SARS-CoV-2 in dynamic seasons |

Given these considerations, the following practices reduce misclassification risk:
- Confirm discordant or high-stakes results with RT-PCR, especially when clinical management or public health actions depend on accuracy[^14].
- Prefer multi-target assays for influenza and SARS-CoV-2 to tolerate genetic drift[^12].
- Implement rigorous quality control and external proficiency testing, particularly for laboratories serving as sentinel sites.

## Co-Infections and Complex Presentations

Co-detection of SARS-CoV-2 with influenza or RSV is well-documented and can complicate clinical interpretation. Multiplex assays are valuable in these scenarios, enabling timely diagnosis and appropriate isolation, treatment decisions, and infection control. Evidence synthesis shows that co-infections occur across age groups, with variability by setting and circulating strains. Importantly, co-detection frequency depends on testing strategy and assay breadth; broader panels naturally increase detection of co-infections compared with single-target assays[^19][^20]. From an epidemiological perspective, this means that increases in ILI or hospitalization with co-detections should not be automatically ascribed to a single pathogen without laboratory confirmation.

## Geographic and Temporal Context

WHO’s week 47 global update places H3N2 predominance across broad geographies, including Northern and South-West Europe, Western, Southern, South-East, and Eastern Asia, the Americas, Eastern and Southern Africa, and Oceania, with co-dominance in Central America and the Caribbean[^1]. EU/EEA summaries and characterizations corroborate these patterns and provide clade-level context, including subclade distribution and antigenic relationships to vaccine strains[^6][^7][^18]. In the U.S., geographic heterogeneity is expected, with some HHS regions showing faster increases than others—consistent with typical seasonal spread dynamics[^8].

WHO’s season summary for October 2024–May 2025 further anchors this year’s activity in a broader temporal frame, showing regional intensity and subtype patterns that inform expectations for the current season[^4]. For the U.S., CDC reports increasing influenza activity primarily among children and young adults, with hospitalizations rising most in older adults—aligning with known H3N2 age-related risk profiles[^8].

## Historical Baseline and Expectations for H3N2

Historically, H3N2 seasons are associated with higher hospitalization burdens in older adults and variable VE due to antigenic drift. Clade dynamics matter: when antigenic drift is substantial, VE can drop, leading to higher rates of medical attendance and hospitalization in certain age groups. Current signals from international and U.S. sources indicate that A(H3) subclade K has increased in prevalence, diverging from the reference strains used in the 2025–26 northern hemisphere vaccine. Early UK and EU/EEA assessments suggest reduced VE against subclade K in adults, consistent with drift-related effects, though protection persists to some degree and remains clinically valuable[^18]. These observations underscore the importance of continuous clade monitoring, antigenic characterization, and VE studies to differentiate between expected H3N2 severity and drift-driven anomalies.

Table 9 summarizes clade dynamics across recent periods.

Table 9. H3N2 Clade Distribution and Antigenic Context (2024–2025 to present)[^6][^7][^18][^4]

| Region/Period | Dominant A(H3) Clades/Subclades | Notes |
|---|---|---|
| EU/EEA (2024–2025 season) | Clade 2a.3a.1; subclade J.2 and descendants | Antigenic characterization against northern hemisphere vaccine references |
| EU/EEA (2025 assessments) | Subclade K emergence and divergence | Significant drift vs. vaccine reference; monitoring recommended |
| Global (Oct 2024–May 2025) | Variable regional dominance | Season summary highlights multi-regional H3N2 intensity |
| U.S. (since May 2025; ongoing) | A(H3) clade 2a.3a.1; subclade K majority since Sept 2025 | Subclade K at 56.9% of characterized A(H3) viruses; antigenic match variable |

## Conclusions and Recommendations

The preponderance of evidence indicates that reported H3N2 activity aligns with influenza surveillance trends in the U.S. and globally. The co-circulation of RSV and low COVID-19 activity provides a consistent backdrop for current respiratory illness patterns. Anomalies identified—most notably the H5N5 death, subclade K mismatch signals, the late September surveillance reporting gap, and diagnostic limitations—are explicable within current knowledge and do not suggest systematic misclassification of H3N2. They do, however, highlight specific risks to surveillance integrity and diagnostic accuracy that warrant proactive mitigation.

Recommendations:
- Maintain high index of suspicion for novel influenza A in severe respiratory cases without clear seasonal influenza dynamics, and verify through sequencing and epidemiological investigation[^8].
- Use multi-target molecular assays and confirmatory PCR for discordant or high-stakes results to reduce false negatives and false positives driven by genetic drift or low viral loads[^12][^14].
- Enhance targeted surveillance where ILI rises without corresponding influenza positivity to capture contributions from RSV, Mycoplasma, and other pathogens; parse ILINet signals in context[^3][^8].
- Strengthen communication around H3N2 subclade K, explaining antigenic drift, expected VE ranges, and the continued value of vaccination for high-risk groups[^18].
- Document and communicate continuity in surveillance reporting to prevent misinterpretation of temporary gaps and maintain public trust[^18].

These actions will improve anomaly detection fidelity, reduce the likelihood of misclassification, and ensure that public health responses remain proportional to the underlying threat landscape.

## Appendix: Data Tables and Calculation Notes

This appendix consolidates key metrics referenced in the analysis, with source notes and caveats. All data are preliminary where indicated and subject to change.

Table 10. Consolidated Metrics (US Week 47; Global Week 47)

| Metric | Value | Source | Caveats |
|---|---:|---|---|
| Global influenza positivity | >20% (Week 47) | WHO Influenza Update[^1] | Aggregated reporting; country-level variability |
| U.S. clinical lab positivity (national) | 5.0% (Week 47) | CDC FluView[^8] | Preliminary; regional range 2.1–14.8% |
| U.S. PHL virus distribution (Week 47) | A: 95.8%; B: 4.2%; A(H3N2) of subtyped A: 82.3% | CDC FluView[^8] | Preliminary; subtype distribution subject to reporting delays |
| U.S. hospitalizations (Oct 1–Nov 22, 2025) | Influenza A: 89.8%; A(H3N2) of subtyped A: 73.5% | CDC FluView[^8] | Preliminary; demographic variation |
| A(H3) genetic characterization (since May 18, 2025) | Subclade K: 56.9% | CDC FluView[^8] | Based on characterized subset; evolving distribution |
| US RSV activity | Increasing; concentrated in children 0–4 in Southeast/South/Mid-Atlantic | CDC Data Channel[^3] | Regional aggregation; may not reflect all local trends |
| US COVID-19 activity | Very Low; wastewater very low | CDC Data Channel[^3] | Lag and sampling variability |
| SARS-CoV-2 VOI/VUM | JN.1 (VOI); KP.3.1.1, LP.8.1, NB.1.8.1, XFG, BA.3.2 (VUMs) | WHO Variant Tracking[^9] | Designations evolve; risk evaluations periodic |

Table 11. Diagnostic Performance Summary (Multiplex LFI vs. PCR)

| Test | Sensitivity (Influenza A) | Specificity (Influenza A) | Key Dependence | Source |
|---|---|---|---|---|
| Microprofit | 82.1% | 97.6% | Decreases at low viral loads (CT ≥30) | Clinical study, ambulatory cohort[^11] |
| Goldsite | 84.9% | 98.0% | Decreases at low viral loads (CT ≥30) | Clinical study, ambulatory cohort[^11] |
| SureScreen | 89.7% | 86.2% | Lower specificity; false-positive risk | Clinical study, ambulatory cohort[^11] |

Calculation notes:
- Percentages are reported as provided by sources; confidence intervals reflect study-specific methods.
- Subclade proportions are computed as the percentage of characterized viruses within a given clade/subclade and do not represent all circulating viruses.
- Activity levels combine multiple CDC indicators (e.g., ED visits, hospitalizations, wastewater), interpreted through CDC’s integrated reporting framework.

---

## References

[^1]: World Health Organization. Current Influenza Update. https://www.who.int/teams/global-influenza-programme/surveillance-and-monitoring/influenza-updates/current-influenza-update
[^2]: European Centre for Disease Prevention and Control. Surveillance for Seasonal Influenza. https://www.ecdc.europa.eu/en/seasonal-influenza/surveillance-reports-and-disease-data
[^3]: Centers for Disease Control and Prevention. Respiratory Illnesses Data Channel. https://www.cdc.gov/respiratory-viruses/data/index.html
[^4]: World Health Organization. Brief Summary of Seasonal Influenza Activity, October 2024–May 2025. https://cdn.who.int/media/docs/default-source/influenza/influenza-updates/summary-reviews-of-influenza-seasons/2025_09_18_influenza-season-summary-report.pdf
[^5]: CDC MMWR. Genomic Surveillance for SARS-CoV-2 Variants. https://www.cdc.gov/mmwr/volumes/73/wr/mm7342a1.htm
[^6]: ECDC. Influenza Virus Characteristics, Week 40 2024 to Week 33 2025. https://www.ecdc.europa.eu/en/publications-data/influenza-virus-characteristics-week-40-2024-week-33-2025
[^7]: ECDC Threat Assessment Brief. Assessing the Risk of Influenza for the EU/EEA (A(H3N2) Subclade K). https://www.ecdc.europa.eu/en/publications-data/threat-assessment-brief-assessing-risk-influenza-november-2025
[^8]: CDC FluView. Weekly US Influenza Surveillance Report: Week 47, 2025. https://www.cdc.gov/fluview/surveillance/2025-week-47.html
[^9]: World Health Organization. Tracking SARS-CoV-2 Variants. https://www.who.int/activities/tracking-SARS-CoV-2-variants
[^10]: ECDC. SARS-CoV-2 Variants of Concern (and current status). https://www.ecdc.europa.eu/en/covid-19/variants-concern
[^11]: Diagnostic Performance of Multiplex Lateral Flow Tests in Ambulatory Patients. https://www.sciencedirect.com/science/article/pii/S0732889324002475
[^12]: Cepheid. Emerging & Evolving Respiratory Viruses: Test Design Matters. https://www.cepheid.com/en-US/insights/insight-hub/respiratory-health/2024/09/emerging-evolving-respiratory-viruses-test-design-matters.html
[^13]: Multiplex Detection of Respiratory RNA Viruses Without Amplification (CRISPR-based). https://pmc.ncbi.nlm.nih.gov/articles/PMC12164057/
[^14]: Simultaneous Detection and Differentiation of SARS-CoV-2 and Other Respiratory Viruses (Co-detection challenges). https://pmc.ncbi.nlm.nih.gov/articles/PMC12428728/
[^15]: CDC Strategy for Enhanced Summer 2024 Influenza Surveillance (APHL). https://www.aphl.org/programs/infectious_disease/influenza/Documents/InfluenzaSurveillanceStrategy_Summer2024_FINAL_053124.pdf
[^16]: CDC Respiratory Virus Activity Levels. https://www.cdc.gov/respiratory-viruses/data/activity-levels.html
[^17]: WHO Data Dashboard. COVID-19 Variants. https://data.who.int/dashboards/covid19/variants
[^18]: CIDRAP. With an Absent CDC and Mismatched “Subclade K” Flu Strain, Experts Face Uncertainty. https://www.cidrap.umn.edu/influenza-vaccines/absent-cdc-and-mismatched-subclade-k-flu-strain-experts-face-upcoming-season
[^19]: Viral Co-detection of Influenza Virus and Other Respiratory Viruses (Frontiers in Microbiology, 2024). https://www.frontiersin.org/journals/microbiology/articles/10.3389/fmicb.2024.1462802/full
[^20]: Co-infections of SARS-CoV-2 with RSV and Influenza A (Study, 2024). https://www.sciencedirect.com/science/article/pii/S2052297524002476
[^21]: WHO Global Respiratory Virus Activity Weekly Update No. 555. https://www.who.int/publications/m/item/global-respiratory-virus-activity--weekly-update-n--555
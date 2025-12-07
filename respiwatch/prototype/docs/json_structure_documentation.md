# Blueprint for a Real-Time H3N2 Outbreak Tracker JSON Dataset and Dashboard Schema (as of 2025-12-06)

## Executive Summary and Objectives

As of early December 2025, the global influenza season is underway and notably earlier than typical in many geographies. Influenza A(H3N2) is dominant or codominant across large swaths of the Northern Hemisphere, with multiple regions showing steep increases in positivity and clear signals of antigenic divergence for the newly emerged A(H3N2) subclade K (J.2.4.1). The World Health Organization (WHO) reports a global positivity above 20% in week 47, with some countries exceeding 30% across Western and Eastern Africa, Northern Europe, and broad sections of Asia; the Western Pacific region posted a sharp regional surge to approximately 35% positivity in the same week. The European Centre for Disease Prevention and Control (ECDC) describes unusually early increases in the European Union/European Economic Area (EU/EEA), driven by subclade K, which accounts for roughly one-third of global A(H3N2) sequences and nearly half within the EU/EEA since May–November 2025, with phylogenetic and serologic evidence of antigenic divergence from the 2025–26 vaccine strain. In the United States, the Centers for Disease Control and Prevention (CDC) shows H3N2 at 82.3% of subtyped influenza A in week 47 and 73.5% of typed A hospitalizations since October 1, with antigenic characterization indicating a mismatch signal and no reduced susceptibility to neuraminidase inhibitors or baloxavir among tested isolates. The United Kingdom’s Health Security Agency (UKHSA) confirms H3N2 predominance and subclade K circulation, measurable hospital admissions in week 48, and corroborating mismatch signals. Media reporting from Japan indicates an early peak attributed to subclade K; however, official national counts remain limited, and these signals should be treated with caution pending confirmation. Altogether, the season’s early timing, H3N2 predominance, and mismatch signals are consistent across agencies and represent the core context for this schema and dataset design.[^1][^2][^3][^4][^5][^6]

This report designs an optimal JSON schema and produces a comprehensive dataset to power a real-time outbreak tracker dashboard optimized for detective epidemiology. The objectives are to:

- Structure core sections—global overview, country-level data, urban centers, pathogen analysis, timeline & progression, intelligence sources, and anomaly detection flags—for efficient ingestion and visualization.
- Embed confidence levels, source attribution, and reliability tiers at the data point level to support transparency and auditability.
- Optimize for low-latency filtering and search across geographies, time, pathogen clades/subclades, and anomaly types.
- Provide a rigorous anomaly detection schema to flag suspicious patterns and support follow-up investigations, while cleanly separating official evidence from OSINT and media claims.

The design emphasizes human-readable naming, metadata-rich records, geolocation support, and versioning for provenance. It also documents information gaps and known limitations—such as U.S. mortality reporting gaps, sparse country-level counts, and the need for real-world vaccine effectiveness (VE) data for subclade K—to ensure cautious interpretation and targeted data enhancement workflows.

[^1]: World Health Organization. Current influenza update.
[^2]: U.S. Centers for Disease Control and Prevention. Weekly US Influenza Surveillance Report: Key Updates for Week 47, ending November 22, 2025.
[^3]: European Centre for Disease Prevention and Control. Threat Assessment Brief: Assessing the risk of influenza for the EU/EEA in the context of increasing circulation of A(H3N2) subclade K (November 2025).
[^4]: UK Health Security Agency. National flu and COVID-19 surveillance report: 4 December 2025 (Week 49).
[^5]: The Japan Times. Japan’s flu season comes early as new variant spreads.
[^6]: WHO Western Pacific. Respiratory Viruses Surveillance Bulletin: Epidemiological Week 47.

## Source Integrity and Methodology

This schema and dataset synthesize official surveillance outputs and risk assessments published through December 5, 2025, including WHO’s global and Western Pacific bulletins, CDC FluView weekly reporting and methods documentation, ECDC’s EU/EEA threat assessment, UKHSA national surveillance, and the Pan American Health Organization’s (PAHO) preparedness alert for the Americas. Where official data were inaccessible (e.g., Canada’s FluWatch+ portal), the gap is documented and excluded from quantitative inference. The design enforces explicit separation between official agency content and media-cited signals (e.g., Japan), which are flagged with lower confidence and pending verification. We use CDC methods documentation to anchor indicator definitions, surveillance scope, and interpretive caveats, including holiday-related reporting delays and preliminary estimates.[^10][^1]

Cross-agency triangulation shows consistent early timing, H3N2 predominance, mismatch signals, and reassuring antiviral susceptibility in tested isolates. The schema’s confidence model incorporates known lags and gaps, such as unavailable U.S. NCHS mortality data for weeks 39–47, sparse country-level counts in WHO updates, and pending real-world VE for subclade K. These limitations are embedded as explicit metadata attributes to prevent over-interpretation and to prioritize follow-up data collection.[^2][^1][^3]

[^10]: U.S. Centers for Disease Control and Prevention. U.S. Influenza Surveillance: Purpose and Methods | FluView.
[^1]: World Health Organization. Current influenza update.
[^2]: U.S. Centers for Disease Control and Prevention. Weekly US Influenza Surveillance Report: Week 47, 2025.
[^3]: European Centre for Disease Prevention and Control. Threat Assessment Brief: Assessing the risk of influenza for the EU/EEA (A(H3N2) subclade K).

## JSON Data Model Overview

The data model organizes information into seven core sections:

1. Global Overview: Aggregated status by continent, positivity patterns, subtype distribution, and key anomalies.
2. Country-Level Data: Country profiles including outbreak status, H3N2 indicators, subclade tracking (especially K), healthcare capacity notes, policy responses, anomaly flags, data confidence, and timestamps.
3. Urban Centers: City/metropolitan focus with outbreak indicators, population density context, transportation hub risk, hospital capacity status, and clustering signals.
4. Pathogen Analysis: Primary pathogen identification (H3N2 vs others), subclade dynamics (K), cross-pathogen context (COVID-19/RSV), laboratory testing accuracy, co-infections, and VE indicators.
5. Timeline & Progression: Week-by-week milestones (weeks 40–49), key events, surveillance gaps, and government responses.
6. Intelligence Sources: Official agency reports and OSINT signals with reliability tiers and audit trails.
7. Anomaly Detection Flags: Structured rules and flags for suspicious patterns, misclassification warnings, information suppression evidence, surveillance gaps, and lab anomalies.

Design principles:

- Human-readable naming and nested metadata for each data point, including source_id, confidence_level, reliability_tier, and last_updated.
- Versioning at dataset and record levels to maintain provenance and auditability.
- Geolocation fields (country_code, ISO3, admin1 where available) to support mapping and spatial filtering.
- Efficient indexing keys (e.g., week, ISO3, city_id, clade, anomaly_type) to power real-time dashboards.
- Clean separation between official and OSINT signals, with explicit reliability tiers to guide visualization and analysis.

Update cadence:

- Weekly synchronization with WHO, CDC, ECDC, and UKHSA; interim updates on significant anomalies (e.g., mismatch confirmation, notable hospital capacity signals).
- OSINT monitoring for city-level capacity signals and media-cited findings, flagged pending verification.

To make the mapping between sources and schema sections explicit, the following table illustrates source-to-section attribution and cadence.

Table S1. Source-to-section mapping and update cadence

| Source (Agency/Report) | Schema Sections Populated | Key Fields | Update Cadence |
|---|---|---|---|
| WHO Global Influenza Update | Global Overview; Timeline; Pathogen Analysis (global context) | Positivity by region; subtype patterns | Weekly[^1] |
| WHO WPRO Surveillance Bulletin | Global Overview (regional surge); Timeline | Regional positivity (e.g., ≈35% week 47) | Weekly[^6] |
| CDC FluView (Week 47) | Country-Level (US); Pathogen Analysis; Timeline; Anomaly Flags | H3N2 proportions; hospitalizations; antigenic characterization; antiviral susceptibility; NCHS gap | Weekly[^2] |
| ECDC Threat Assessment (Subclade K) | Global Overview (EU/EEA); Country-Level (EU/EEA aggregate); Pathogen Analysis | Subclade proportions; mismatch; risk context | Periodic, as updated[^3] |
| UKHSA National Surveillance (Week 49) | Country-Level (UK); Urban Centers (selected cities); Pathogen Analysis; Timeline | H3N2 positives; hospital admissions; genetic characterization; antigenic mismatch; antiviral markers | Weekly[^4] |
| PAHO Epidemiological Alert | Global Overview (Americas preparedness); Country-Level (aggregate messaging) | Preparedness guidance; co-circulation notes | Periodic alerts[^7] |
| Ontario Respiratory Virus Tool | Urban Centers (Northern Ontario) | Activity levels (contextual signals) | Regular updates[^28] |
| Media (Japan Times) | Urban Centers (Japan cities); Intelligence Sources | Early peak signals; subclade K attribution; small-sample prevalence | As published; flagged pending verification[^5] |
| Social Media (e.g., Adelaide ED status) | Urban Centers (Adelaide); Anomaly Flags | ED overcrowding narratives | Continuous; reliability tiering applied[^26] |
| Additional OSINT (NBC News, CBC, ABC, ACEM) | Urban Centers; Intelligence Sources | Cross-jurisdiction surge and capacity signals | As published; tiered reliability[^22][^27][^25] |

### Global Overview Section

The global overview captures a consolidated snapshot by continent, reflecting early-season acceleration and H3N2 predominance. It summarizes positivity, subtype patterns, key anomalies, and surveillance system status. This section harmonizes WHO global patterns with EU/EEA-specific risk context and WPRO surge indicators, providing a high-level view that informs the rest of the dataset.[^1][^3][^6]

### Country-Level Data

Country profiles store outbreak status, H3N2 case indicators, hospitalization metrics, subclade distribution (with emphasis on K), healthcare capacity notes, policy responses (e.g., vaccination campaigns), anomaly flags, and confidence levels. The schema includes structured fields to accommodate antigenic mismatch signals and antiviral susceptibility where available. For the U.S., CDC data populate virologic and clinical indicators with clear acknowledgement of NCHS mortality gaps; UKHSA data underpin UK fields including week 48 hospital admissions and genetic characterization; ECDC inputs reflect EU/EEA aggregate risk framing and subclade proportions.[^2][^4][^3]

Table S2. Country data dictionary (selected fields)

| Field | Type | Description | Example |
|---|---|---|---|
| country_name | string | Country name | United States |
| iso3 | string | ISO-3166 alpha-3 | USA |
| outbreak_status | enum | Current status | active_outbreak |
| h3n2_cases_week | integer | Weekly H3N2 cases (where available) | 251 |
| h3n2_prop_subtyped_a | number | Proportion of H3N2 among subtyped A | 0.823 |
| hospitalizations_cumulative | integer | Cumulative hospitalizations (season-to-date) | 905 |
| h3n2_hosp_prop_typed_a | number | Proportion H3N2 among typed A hospitalizations | 0.735 |
| subclade_k_prop | number | Proportion of H3N2 characterized as K | 0.82 |
| vaccine_mismatch_signal | boolean | Antigenic mismatch indicator | true |
| antiviral_susceptibility | enum | Summary of susceptibility | susceptible |
| policy_responses | array | List of interventions with status | ["vaccination_acceleration"] |
| anomaly_flags | array | Country-specific anomalies | ["mortality_reporting_gap"] |
| confidence_level | enum | Data confidence | medium |
| last_updated | string (ISO datetime) | Last update timestamp | 2025-12-05T00:00:00Z |

### Urban Centers Section

The urban centers section enables city-level situational awareness, linking outbreak indicators with population density, transportation hub risk assessments, hospital capacity status, and clustering patterns. Notable hotspots include Adelaide, where ED overcrowding and “hidden ramping” have been reported by multiple sources, and Northern Ontario, where early surges atop chronic overcrowding are documented. The schema flags OSINT and media claims with pending verification status and explicitly labels official confirmations.[^26][^27][^25][^28]

Table S3. City metadata fields

| Field | Type | Description | Example |
|---|---|---|---|
| city_id | string | Unique city identifier | AUS_ADELAIDE |
| city_name | string | City name | Adelaide |
| country_iso3 | string | Country code | AUS |
| population_density | number | Residents per km² (where available) | — |
| transport_hub_risk | enum | Risk assessment | high |
| hospital_capacity_status | enum | ED capacity status | critical_overload |
| weekly_cases | integer | Weekly case counts (where available) | 81 |
| weekly_hospital_admissions | integer | Weekly H3N2 admissions | 48 |
| anomaly_flags | array | City-specific anomalies | ["hidden_ramping"] |
| confidence_level | enum | Data confidence | high |
| verification_status | enum | Evidence verification | verified |

### Pathogen Analysis Section

Pathogen analysis captures H3N2 predominance and subclade dynamics, especially K. It integrates cross-pathogen context (SARS-CoV-2 variants and RSV activity), laboratory testing accuracy, co-infection patterns, and vaccine effectiveness signals. To support cautious interpretation, the schema includes mismatch indicators, reliability tiers, and explicit source attribution. The section also records antiviral susceptibility results and notable genetic markers (e.g., NA K249E, PA I38L in UK reporting).[^3][^2][^4][^8][^9]

Table S4. Pathogen metrics dictionary

| Field | Type | Description | Example |
|---|---|---|---|
| primary_pathogen | enum | Dominant pathogen | H3N2 |
| subclade_k_prop | number | Proportion of H3N2 characterized as K | 0.5 |
| vaccine_mismatch_signal | boolean | Antigenic mismatch indicator | true |
| ve_estimate | number | Interim VE estimate (%) | — |
| ve_confidence_interval | array | Lower and upper bounds | [20, 45] |
| antiviral_susceptibility | enum | Susceptibility summary | susceptible |
| co_infections | array | Co-detected pathogens | ["RSV", "SARS-CoV-2"] |
| testing_accuracy | object | Modality sensitivity/specificity | {"LFI": {"sens": 0.85, "spec": 0.9}} |
| cross_pathogen_context | object | COVID-19/RSV indicators | {"COVID_activity": "low", "RSV_activity": "rising"} |
| reliability_tier | enum | Source reliability | official |

### Timeline & Progression

A week-by-week timeline (weeks 40–49) records progression and key events: global positivity exceeding 20% in week 47, U.S. H3N2 dominance in public health labs, WPRO’s ≈35% positivity, and UK hospital admissions in week 48. It also logs surveillance gaps (e.g., U.S. NCHS mortality data unavailable weeks 39–47) and major government responses or guidance. This timeline anchors dataset updates and supports anomaly investigations.[^1][^2][^4][^6][^7]

Table S5. Timeline index (weeks 40–49)

| Week | Milestone | Indicator | Source |
|---|---|---|---|
| 40–45 | Early rise in NH | Elevated positivity; low-level increases | WHO[^1] |
| 46 | U.S. ILI stable | 2.2% outpatient ILI; low clinical lab positivity | CDC week 46[^9] |
| 47 | Global surge signal | >20% global positivity; WPRO ≈35% | WHO; WPRO[^1][^6] |
| 47 | U.S. H3N2 dominant | 82.3% of subtyped A; mismatch signal | CDC[^2] |
| 48 | UK admissions measurable | 48 hospital; 9 ICU/HDU; 81 H3N2 positives | UKHSA[^4] |
| 49 | EU/EEA early increases | Detections ~3–4 weeks early | ECDC[^3] |

### Intelligence Sources

Intelligence sources are captured with reliability tiers and audit trails. Official agencies (WHO, CDC, ECDC, UKHSA, PAHO, WPRO) provide the backbone; OSINT and media signals augment city-level awareness, especially where official dashboards are limited or delayed. The schema marks claims about hospital capacity, early peaks, and mismatches with verification status and provenance fields, distinguishing official confirmations from tier-2/3 reports. OSINT handling follows established guidance on triangulation and credibility assessment.[^21][^22][^23][^24][^15]

Table S6. Intelligence reliability rubric

| Tier | Source Type | Examples | Usage Guidance |
|---|---|---|---|
| 1 | Official agencies; peer-reviewed | WHO, CDC, UKHSA, ECDC, PAHO; peer-reviewed studies | Anchor conclusions and policy |
| 2 | Major media; recognized experts | CBC, ABC, CNN, NBC, CIDRAP | Contextualize and add detail |
| 3 | Social and alternative media | Twitter/X, blogs | Signal detection; verify before escalation |

### Anomaly Detection Flags

The anomaly schema codifies suspicious patterns and structured flags for:

- Vaccine mismatch signals (subclade K vs vaccine strain).
- Potential misclassification warnings (testing limitations, discordant ILI vs positivity).
- Information suppression or surveillance gaps (e.g., U.S. NCHS mortality unavailable weeks 39–47).
- Laboratory anomalies (rare markers, assay performance issues).
- City-level surge and capacity stress (e.g., Adelaide, Northern Ontario).

Each flag includes severity, confidence, explanation, supporting sources, and recommended follow-up. This structure supports triage and investigation while maintaining evidentiary provenance.[^2][^4][^3][^26][^27][^25]

Table S7. Anomaly taxonomy and severity matrix

| Category | Description | Severity | Example |
|---|---|---|---|
| Mismatch | Antigenic divergence vs vaccine | high | Subclade K mismatch[^3][^2] |
| Surveillance gap | Missing mortality data | high | U.S. NCHS weeks 39–47[^2] |
| Capacity stress | ED overcrowding | critical | Adelaide ED (>125% capacity)[^26][^25] |
| Lab anomaly | Rare antiviral markers | medium | NA K249E; PA I38L[^4] |
| Misclassification | ILI vs positivity discordance | medium | Syndromic overlap[^2][^8] |

## Global Overview Synthesis (What’s happening)

The week 47 global snapshot indicates early-season acceleration across the Northern Hemisphere with H3N2 predominance. WHO reports global positivity above 20%, with multiple subregions exceeding 30%. WPRO’s ≈35% positivity underscores early surge dynamics in the Western Pacific. In the U.S., H3N2 is dominant among subtyped influenza A (82.3% in week 47) and accounts for most typed A hospitalizations; antigenic characterization suggests potential reduced vaccine match, while antiviral susceptibility remains reassuring. EU/EEA data highlight subclade K’s growing footprint and early increases relative to typical seasonality. UKHSA confirms H3N2 majority subtype, subclade K predominance among characterized viruses, measurable hospital admissions, and low reactivity in antigenic characterization. PAHO emphasizes preparedness for concurrent circulation of influenza, RSV, and SARS-CoV-2, and UK interim VE analyses provide early-season effectiveness context.[^1][^6][^2][^3][^4][^7][^8]

Table 1. At-a-glance H3N2 snapshot (decoded into schema fields)

| Geography | Dominant strain and signals | Timing vs typical | Key quantitative indicators | Outbreak status | Data gaps |
|---|---|---|---|---|---|
| Global (WHO) | H3N2 predominant/codominant; >20% positivity; >30% in some countries | Earlier than usual | Week 47 positivity >20% | Season underway earlier | Limited country-level counts[^1] |
| United States (CDC) | H3N2 82.3% of subtyped A; mismatch signal; no antiviral resistance | Early-season rise | 1.1M illnesses; 11K hospitalizations; 450 deaths; H3N2 73.5% of typed A hospitalizations | Low but increasing | NCHS mortality data unavailable weeks 39–47[^2] |
| EU/EEA (ECDC) | Subclade K ≈33% global; ≈50% EU/EEA of A(H3N2) | Detections ~3–4 weeks early | Sequence proportions; limited case counts | Early-wave concerns | Real-world VE pending[^3] |
| England (UKHSA) | H3N2 dominant; subclade K predominates; antigenic mismatch | Earlier increase | Week 48: 81 H3N2 positives; 48 hospital admissions; 9 ICU/HDU admissions | Moderate early increases | Subtype-specific mortality not reported[^4] |
| Japan (Media citing surveillance) | “Subclade K” reported in small samples | Early peak reported | Small-sample surveillance | Early surge signals | Rely on official ministry data[^5] |
| Western Pacific (WPRO) | Sharp increase in positivity | Early acceleration | Positivity ≈35% (week 47) | Early surge | Limited subtype breakdown[^6] |
| Americas (PAHO) | Guidance for co-circulation of influenza, RSV, SARS-CoV-2 | Preparedness focus | Qualitative guidance | Monitoring onset | H3N2 counts not in latest alert[^7] |

WHO Week 47 positivity and subtype distribution further codify regional patterns (Table 2), aligning with the dashboard’s global overview fields and supporting real-time filtering by region and subtype predominance.[^1][^6]

Table 2. WHO Week 47 positivity and subtype patterns

| Geography | Positivity status | Subtype predominance/codominance | Notes |
|---|---|---|---|
| Global | >20% positivity | H3N2 predominant/codominant in many Northern Hemisphere regions | Southern Hemisphere largely low[^1] |
| Northern Europe | Elevated; some >30% | H3N2 predominant | Early increase[^1] |
| Western & Southern Europe | Elevated | H3N2 predominant | Early season[^1] |
| Western Asia | Elevated | H3N2 predominant | Early season[^1] |
| Southern & Eastern Asia | Elevated; some >30% | H3N2 predominant/codominant | Early acceleration[^1] |
| South-East Asia | Elevated | H3N2 predominant | Early acceleration[^1] |
| North/Middle Africa | Elevated | H1N1pdm09 predominant | Distinct subtype pattern[^1] |
| Central America & Caribbean | Increasing | Codominant H3N2 and H1N1pdm09 | Early activity[^1] |
| Eastern & Southern Africa | Scattered elevated | H3N2 predominant | Mixed signals[^1] |
| Oceania | Some elevated | H3N2 predominant | Early signs[^1] |
| Western Pacific (regional) | ≈35% positivity (week 47) | Not broken down by subtype | Early surge[^6] |

## Country-Level Deep Dives (How and so-what)

The country sections translate agency findings into structured dashboard fields, emphasizing H3N2 dominance, subclade tracking, mismatch signals, hospitalization trends, and policy responses. Confidence levels and last-updated timestamps are mandatory. The design anticipates weekly refreshes and interim anomaly updates.

### United States (CDC)

CDC’s week 47 data indicate H3N2 predominance in public health laboratory detections (82.3% of subtyped A) and 73.5% of typed A hospitalizations since Oct 1. Antigenic characterization shows potential reduced vaccine match (33.3% well recognized), while antiviral susceptibility remains reassuring across tested isolates. Preliminary cumulative estimates include at least 1.1 million illnesses, 11,000 hospitalizations, and 450 deaths; notably, NCHS mortality surveillance data for weeks 39–47 were not available, constraining mortality analysis.[^2]

Table 3. CDC week 47 virologic summary (schema fields)

| Indicator | Week 47 | Cumulative since Week 40 | Schema Field |
|---|---|---|---|
| Influenza A subtyped (public health labs) | 305 | 2,159 | h3n2_cases_week; h3n2_cases_cumulative |
| H3N2 proportion (of subtyped A) | 82.3% | 74.6% | h3n2_prop_subtyped_a |
| Genetic characterization (H3N2) | 55 viruses; 82% subclade K | — | subclade_k_prop |
| Antigenic characterization | 60 H3N2 tested; 33.3% well recognized | — | vaccine_mismatch_signal: true |
| Antiviral resistance | 0% reduced susceptibility (NIs); 0% decreased (baloxavir) | — | antiviral_susceptibility: susceptible |

Table 4. CDC clinical and mortality indicators (cumulative)

| Indicator | Value | Period | Schema Field |
|---|---|---|---|
| Hospitalizations (FluSurv-NET) | 905 | Oct 1–Nov 22 | hospitalizations_cumulative |
| H3N2 proportion among typed A | 73.5% | Oct 1–Nov 22 | h3n2_hosp_prop_typed_a |
| Estimated illnesses | ≥1,100,000 | Season-to-date | estimated_illnesses |
| Estimated hospitalizations | 11,000 | Season-to-date | estimated_hospitalizations |
| Estimated deaths | 450 | Season-to-date | estimated_deaths |
| NCHS mortality surveillance | Not available | Weeks 39–47 | anomaly_flag: mortality_reporting_gap |

So-what: The U.S. picture combines early-season rise with mismatch signals and reassuring antiviral susceptibility. The mortality gap and limited subnational counts complicate real-time severity assessment. The dashboard must flag these constraints and prioritize enhanced sampling and VE monitoring.

### United Kingdom (UKHSA)

UKHSA confirms H3N2 majority subtype and subclade K predominance among characterized viruses, with measurable hospital admissions in week 48. Antigenic characterization indicates low reactivity for many K-clade viruses, aligning with mismatch signals. Small numbers of detections carry markers potentially associated with reduced susceptibility (NA K249E; PA I38L). Subtype-specific mortality has not been reported.[^4]

Table 5. UKHSA week 48 key indicators (schema fields)

| Indicator | Value | Period | Schema Field |
|---|---|---|---|
| H3N2 positives (Respiratory DataMart) | 81 / 4,154 tested | Week 48 | weekly_cases |
| H3N2 genetically characterized | 451 total; 420 subclade K | Weeks 40–48 | subclade_k_prop |
| New hospital admissions (A(H3N2)) | 48 | Week 48 | weekly_hospital_admissions |
| New ICU/HDU admissions (A(H3N2)) | 9 | Week 48 | weekly_icu_hdu_admissions |
| Antigenic characterization | 6/7 (K-clade) low reactivity | Weeks 40–48 | vaccine_mismatch_signal: true |
| Antiviral susceptibility markers | NA K249E; PA I38L (<5 each) | Weeks 40–48 | lab_anomaly_flags |

So-what: UK data provide the most granular view of early H3N2 dynamics with clade-level resolution, hospital admissions, and mismatch corroboration. The schema encodes these as high-confidence official signals, complemented by targeted anomaly flags.

### EU/EEA (ECDC)

ECDC’s threat assessment highlights early increases and significant antigenic divergence of subclade K from the vaccine strain. Subclade K accounts for approximately one-third of global A(H3N2) sequences and nearly half in the EU/EEA (May–November 2025). While vaccination is expected to mitigate severe disease, real-world VE data for subclade K remain pending.[^3]

Table 6. ECDC risk assessment highlights (schema fields)

| Dimension | Summary | Schema Field |
|---|---|---|
| Circulation timing | Detections ~3–4 weeks earlier than usual | season_timing_anomaly: true |
| Subclade K proportions | ≈33% global; ≈50% EU/EEA of A(H3N2) | subclade_k_prop_region |
| Vaccine match | Significant divergence; mismatch indicated | vaccine_mismatch_signal: true |
| Expected severity | No unusual severity reported (East Asia declines) | severity_expected: typical |
| Public health actions | Urgent vaccination; anticipate early surge | policy_responses: ["vaccination_acceleration"] |

So-what: EU/EEA risks are driven by timing and mismatch rather than unusual severity. The dashboard should foreground mismatch signals, monitor VE studies, and emphasize vaccination uptake.

### Japan

Media reporting indicates an early peak and high prevalence of “subclade K” in a small sample. The schema flags such claims as medium-to-low confidence pending official confirmation and encodes them in urban centers with verification_status: pending. This ensures visibility without over-weighting unverified data.[^5]

Table 7. Japan surveillance signals (schema fields)

| Indicator | Value | Schema Field |
|---|---|---|
| Sample prevalence (K) | ≈96% in small sample (22/23) | subclade_k_prop_city (pending) |
| Official national counts | Limited in provided context | verification_status: pending |
| Source reliability | Media citing surveillance | reliability_tier: 2 |

So-what: Japan’s city-level signals should be displayed with caution. The dashboard’s OSINT flags and verification badges help prevent misinterpretation until official counts are available.

### Western Pacific (WHO WPRO)

WPRO reports a sharp increase in influenza positivity to approximately 35% in week 47. While subtype breakdowns are limited, this magnitude indicates early surge dynamics and justifies elevated status in the global overview.[^6]

Table 8. WPRO positivity (schema fields)

| Indicator | Value | Schema Field |
|---|---|---|
| Regional positivity | ≈35% (week 47) | regional_positivity: 0.35 |
| Subtype breakdown | Not provided | subtype_breakdown: null |

So-what: The Western Pacific is a key early surge node. The dashboard should present elevated positivity with transparent subtype data gaps.

### Americas (PAHO)

PAHO’s alert focuses on preparedness for concurrent circulation of influenza, RSV, and SARS-CoV-2, emphasizing surveillance strengthening, vaccination of high-risk groups, and adequate supplies of antivirals and PPE. The alert does not provide H3N2-specific counts, which are recorded as null fields with confidence level: high (guidance) but low (count data).[^\[7\]]

Table 9. PAHO guidance and status fields

| Dimension | Summary | Schema Field |
|---|---|---|
| Preparedness | Co-circulation readiness | policy_responses: ["readiness_plan"] |
| Surveillance | Strengthen influenza/RSV/SARS-CoV-2 | surveillance_enhancement: true |
| Vaccination | High-risk groups | vaccination_focus: ["elderly", "comorbidities"] |
| H3N2 counts | Not provided in latest alert | h3n2_cases_week: null |

So-what: The Americas are in transition. The dashboard should present guidance and preparedness indicators while flagging missing H3N2 counts.

### Additional Countries (China, Canada, Australia)

China’s weekly portal confirms active surveillance; detailed H3N2 counts were not extracted. Canada’s FluWatch+ portal was inaccessible; no counts were incorporated. Australia’s official reports exist, but counts were not retrieved in this review. These limitations are encoded in the schema as data gaps with confidence levels and recommended follow-up.[^13][^7][^11]

Table 10. Official signals by country (schema fields)

| Country/Region | Agency | Status | Schema Fields |
|---|---|---|---|
| China | China CDC | Active surveillance | surveillance_status: active; detailed_counts: null |
| Canada | PHAC | Portal inaccessible | surveillance_status: limited; data_gaps: ["fluwatch_inaccessible"] |
| Australia | DoH | Official reports exist | surveillance_status: active; detailed_counts: null |

So-what: These gaps shape the dataset’s global coverage. The dashboard should present active surveillance markers while transparently encoding absent counts.

## Urban Centers Situational Awareness

City-level signals matter because hospital capacity and transmission dynamics often manifest locally before national dashboards register change. The schema prioritizes ED overcrowding, hospital admissions, positivity trends, clustering signals, and verification status. OSINT inputs are flagged and triangulated against official statements where possible.[^26][^27][^25]

Table 11. ED overcrowding summary (schema mapping)

| Location | Capacity Indicator | Drivers | Sources | Schema Fields |
|---|---|---|---|---|
| Adelaide (RAH) | >125% ED capacity; all spaces occupied by admitted inpatients | Bed block; aged care placement delays; hidden ramping | Clinician posts; institutional statements; professional body reports | city_id; hospital_capacity_status: critical_overload; anomaly_flags: ["hidden_ramping"]; verification_status: verified[^26][^25] |
| Northern Ontario | Early seasonal surge atop chronic overcrowding | Influenza and COVID co-circulation; throughput pressures | Provincial tool; local media | city_id; hospital_capacity_status: strained; anomaly_flags: ["early_surge"]; verification_status: verified[^28][^27] |

So-what: Adelaide and Northern Ontario are emblematic of early-season stress. The dashboard should elevate capacity flags and present them alongside transport hub risk and population density, where available.

## Pathogen Analysis and Cross-Pathogen Context

Pathogen analysis consolidates H3N2 predominance, subclade K dynamics, and vaccine mismatch signals, while situating influenza within a broader respiratory context (RSV rising, COVID-19 low). Laboratory testing performance and co-infection patterns inform misclassification risk and interpretation of syndromic data.

Table 12. Consolidated Metrics (schema mapping)

| Metric | Value | Source | Schema Field |
|---|---|---|---|
| Global influenza positivity | >20% (Week 47) | WHO | global_positivity: 0.2 |
| U.S. clinical lab positivity (national) | 5.0% (Week 47) | CDC | national_positivity_us: 0.05 |
| U.S. PHL virus distribution (Week 47) | A: 95.8%; B: 4.2%; A(H3N2) of subtyped A: 82.3% | CDC | h3n2_prop_subtyped_a: 0.823 |
| U.S. hospitalizations (Oct 1–Nov 22) | A: 89.8%; A(H3N2) of subtyped A: 73.5% | CDC | hospitalizations_cumulative; h3n2_hosp_prop_typed_a: 0.735 |
| A(H3) genetic characterization (since May 18, 2025) | Subclade K: 56.9% | CDC | subclade_k_prop: 0.569 |
| US RSV activity | Increasing (children 0–4) | CDC Data Channel | cross_pathogen_context: {"RSV": "rising"} |
| US COVID-19 activity | Very low; wastewater very low | CDC Data Channel | cross_pathogen_context: {"COVID": "very_low"} |
| SARS-CoV-2 VOI/VUM | JN.1 (VOI); KP.3.1.1, LP.8.1, NB.1.8.1, XFG, BA.3.2 (VUMs) | WHO Variant Tracking | covid_variants: ["JN.1", "KP.3.1.1", "LP.8.1", "NB.1.8.1", "XFG", "BA.3.2"] |

Table 13. SARS-CoV-2 variant status (schema mapping)

| Designation | Lineage | Features | Schema Field |
|---|---|---|---|
| VOI | JN.1 | BA.2.86 + S:L455S | covid_variants_voi: ["JN.1"] |
| VUM | KP.3.1.1 | KP.3 + S:S31- | covid_variants_vum: ["KP.3.1.1"] |
| VUM | LP.8.1 | Multiple spike changes | covid_variants_vum: ["LP.8.1"] |
| VUM | NB.1.8.1 | JN.1 + S:T22N, S:F59S, S:F456L | covid_variants_vum: ["NB.1.8.1"] |
| VUM | XFG | JN.1 + S:T22N, S:S31P, S:R346T, S:F456L | covid_variants_vum: ["XFG"] |
| VUM | BA.3.2 | Extensive mutation profile | covid_variants_vum: ["BA.3.2"] |

Laboratory testing performance varies by modality. Multiplex lateral flow tests show variable sensitivity and specificity, with performance declining at low viral loads. Some tests underperform WHO minimum specificity criteria, raising false-positive risks. The schema encodes these performance characteristics and recommended confirmatory pathways to reduce misclassification.[^17][^18][^19]

Table 14. Diagnostic performance summary (schema mapping)

| Test | Sensitivity | Specificity | Dependence | Schema Field |
|---|---|---|---|---|
| Microprofit | 82.1% | 97.6% | Decreases at CT ≥30 | testing_accuracy: {"LFI": {"sens": 0.821, "spec": 0.976}} |
| Goldsite | 84.9% | 98.0% | Decreases at CT ≥30 | testing_accuracy: {"LFI": {"sens": 0.849, "spec": 0.98}} |
| SureScreen | 89.7% | 86.2% | Lower specificity | testing_accuracy: {"LFI": {"sens": 0.897, "spec": 0.862}} |

Co-infections are documented across influenza, RSV, and SARS-CoV-2. Multiplex assays facilitate detection but can increase co-detection frequency depending on panel breadth. Interpretively, rising ILI without corresponding influenza positivity may reflect RSV or other pathogens; the schema supports anomaly flags to encourage targeted testing and cautious attribution.[^20][^19]

## Timeline of Key Epidemiological Events (Weeks 40–49, 2025)

The timeline codifies milestones and early indicators, providing a backbone for dataset versioning and anomaly investigations. It documents global positivity thresholds, H3N2 dominance signals, regional surges, and hospital admissions.

Table 15. Milestones timeline (schema fields)

| Week | Milestone | Indicator | Source | Schema Field |
|---|---|---|---|---|
| 40–45 | Early rise in NH | Elevated positivity; low-level increases | WHO | global_phase: "early_rise" |
| 46 | U.S. ILI stable | 2.2% outpatient ILI; low clinical lab positivity | CDC week 46 | us_ili_percent: 0.022 |
| 47 | Global surge signal | >20% global positivity; WPRO ≈35% | WHO; WPRO | global_positivity: 0.2; regional_positivity_wpro: 0.35 |
| 47 | U.S. H3N2 dominant | 82.3% of subtyped A; mismatch signal | CDC | h3n2_prop_subtyped_a: 0.823; vaccine_mismatch_signal: true |
| 48 | UK admissions measurable | 48 hospital; 9 ICU/HDU; 81 H3N2 positives | UKHSA | weekly_hospital_admissions: 48; weekly_cases: 81 |
| 49 | EU/EEA early increases | Detections ~3–4 weeks early | ECDC | season_timing_anomaly: true |

## Anomaly Detection Framework and Suspicious Patterns

The anomaly schema translates cross-agency consistencies and discrepancies into structured flags. Vaccine mismatch signals are consistent across CDC, ECDC, and UKHSA. U.S. mortality reporting gaps (NCHS weeks 39–47) warrant explicit flagging. Laboratory testing limitations can produce false negatives/positives, and ILI vs influenza positivity discordance can mislead interpretation. City-level surge signals are triangulated with official statements where possible.[^3][^2][^4][^26][^27][^25]

Table 16. Anomaly matrix (schema mapping)

| Signal/Observation | Expected Pattern | Observed | Explanation | Follow-up Actions |
|---|---|---|---|---|
| Subclade K mismatch | Some drift expected | Significant divergence; mismatch | Antigenic drift vs vaccine strain | Monitor antigenic characterization; adjust communications; encourage vaccination[^3][^2] |
| U.S. mortality gap | Continuous reporting | NCHS unavailable weeks 39–47 | Operational/reporting interruption | Document continuity; communicate transparently[^2] |
| LFI false positives/negatives | High performance at high load; lower at low load | Variability; specificity gaps | Viral load dependence; assay limits | Confirm discordant results with PCR; prefer multi-target assays[^17][^18] |
| ILI rises without influenza positivity | Mixed pathogen circulation | ILI may rise due to RSV/Mycoplasma/pertussis | Syndromic overlap | Increase targeted testing; parse ILI by pathogen[^2][^8] |
| ED overcrowding | Capacity threshold breaches | Adelaide >125%; Northern Ontario strain | Bed block; aged care delays; co-circulation | Triangulate via institutional statements; escalate monitoring[^26][^27][^25] |

## Intelligence Sources, Reliability, and Provenance

The dashboard encodes source reliability using tiering and explicit provenance. Official agency outputs (WHO, CDC, ECDC, UKHSA, PAHO, WPRO) are Tier 1 anchors. Major media and recognized expert platforms (CBC, ABC, CNN, NBC, CIDRAP) provide Tier 2 context. Social and alternative media (Twitter/X) are Tier 3 and used for signal detection pending verification. OSINT management follows triangulation protocols to avoid overweighting unverified claims.[^21][^22][^23][^24][^15]

Table 17. Source reliability rubric (schema mapping)

| Tier | Source Type | Examples | Dashboard Usage |
|---|---|---|---|
| 1 | Official; peer-reviewed | WHO, CDC, UKHSA, ECDC, PAHO; peer-reviewed | Primary visualization and analysis |
| 2 | Major media; experts | CBC, ABC, CNN, NBC, CIDRAP | Context and corroboration |
| 3 | Social/alternative | Twitter/X, blogs | Early signals; pending verification |

Provenance metadata (source_id, reliability_tier, verification_status, last_updated) is mandatory for all records. OSINT records carry explicit disclaimers and are excluded from core指标 calculations unless corroborated by Tier 1–2 sources.

## Visualization, Filtering, and Real-Time Update Strategy

The schema defines indexing keys to power low-latency filtering and search: week, ISO3, clade/subclade, anomaly_type, and city_id. Region selectors and pathogen filters (H3N2, subclade K, RSV, COVID-19) enable comparative views. Anomaly overlays allow dashboards to surface mismatch signals, capacity stress, and testing limitations with severity and confidence badges. Update cadence mirrors agency schedules with interim anomaly bursts.

Table 18. Dashboard filter dictionary

| Filter | Field | Type | Example |
|---|---|---|---|
| Week | week | integer | 47 |
| Region | region | enum | "Western Pacific" |
| Country (ISO3) | iso3 | string | "USA" |
| City | city_id | string | "AUS_ADELAIDE" |
| Pathogen | pathogen | enum | "H3N2" |
| Subclade | clade_subclade | string | "K (J.2.4.1)" |
| Anomaly type | anomaly_type | enum | "vaccine_mismatch" |
| Reliability | reliability_tier | enum | 1 |
| Verification | verification_status | enum | "verified" |

Indexing strategy emphasizes compound keys for time-series views (week + ISO3) and geospatial mapping (ISO3 + city_id). Pathogen filters rely on normalized enumerations to avoid ambiguity.

## Data Quality, Limitations, and Enhancement Roadmap

Key limitations and their operational implications:

- U.S. NCHS mortality data unavailable weeks 39–47; holiday-related reporting delays. Mitigation: flag as high-severity gap, supplement with alternative mortality indicators, and communicate uncertainty.[^2]
- Sparse country-level counts and incomplete subtype breakdowns in WHO regional reports; WPRO lacks subtype granularity in week 47. Mitigation: use positivity thresholds and predominance patterns with confidence badges.[^1][^6]
- Media vs official source discrepancies (Japan early peak); reliance on small samples. Mitigation: city-level signals marked pending verification; prioritize official ministry dashboards.[^5]
- Canada surveillance portal inaccessible; Australia/China detailed counts not extracted. Mitigation: encode gaps, maintain active surveillance flags, and plan targeted retrieval.[^7][^11][^13]
- Real-world VE for subclade K pending; early signals suggest mismatch. Mitigation: track interim VE updates, antigenic characterization, and severity patterns.[^3][^8]

Validation checks:

- Cross-indicator consistency: positivity vs subtyping proportions vs hospitalization trends.
- Temporal continuity: gap flags and resumption markers.
- Source corroboration: minimum two independent sources for escalation.
- Laboratory performance: monitor assay-specific sensitivity/specificity and false result risks.

Roadmap:

- Expand city-level mapping with geocoding, population density, and transport hub metadata.
- Integrate wastewater surveillance streams to complement clinical indicators.
- Automate provenance tracking and verification workflows; enhance anomaly triage.
- Add real-time VE studies and antigenic characterization as they become available.

Table 19. Data gaps and mitigation plan

| Gap | Impact | Mitigation | Priority |
|---|---|---|---|
| U.S. NCHS mortality weeks 39–47 | Severity assessment constrained | Flag; use alternative indicators; transparent comms | High[^2] |
| Country-level counts sparse | Cross-country comparison limited | Encode positivity patterns; mark confidence | High[^1] |
| WPRO subtype breakdown missing | Pathogen attribution uncertainty | Regional positivity only; mark subtype null | Medium[^6] |
| Canada portal inaccessible | National visibility reduced | Encode gap; engage provincial tools | Medium[^7] |
| Japan official counts limited | Early peak claims uncertain | Mark pending; seek ministry confirmation | High[^5] |
| VE for subclade K pending | Mismatch impact unclear | Track interim VE; update schema fields | High[^3][^8] |

## Appendices: Schema Field Dictionary and Enumerations

To facilitate implementation, the schema provides a field dictionary with required/optional flags, types, allowed enumerations, and examples. Common enumerations include severity (low, medium, high, critical), confidence_level (high, medium, low), reliability_tier (1, 2, 3), outbreak_status (active_outbreak, elevated_activity, normal, unknown), and transport_hub_risk (low, medium, high, critical). Units are explicit for percentages (0–1), rates (per 100,000), and counts (integer). The metadata block includes dataset_version and last_updated for provenance.

Table 20. Schema field dictionary (selected)

| Field | Type | Required | Allowed Values | Example | Notes |
|---|---|---|---|---|---|
| dataset_version | string | yes | semver | "2025.12.06" | Dataset provenance |
| last_updated | string (ISO datetime) | yes | — | "2025-12-06T00:00:00Z" | Update tracking |
| region | string | yes | WHO regions | "Western Pacific" | Global overview |
| country_name | string | yes | — | "United States" | Country section |
| iso3 | string | yes | ISO-3166 alpha-3 | "USA" | Geolocation |
| outbreak_status | enum | yes | active_outbreak; elevated_activity; normal; unknown | "active_outbreak" | Status summary |
| h3n2_cases_week | integer | no | ≥0 | 251 | Week-specific |
| h3n2_prop_subtyped_a | number | yes | 0–1 | 0.823 | Core indicator |
| hospitalizations_cumulative | integer | no | ≥0 | 905 | Clinical burden |
| h3n2_hosp_prop_typed_a | number | no | 0–1 | 0.735 | Hospitalization mix |
| subclade_k_prop | number | no | 0–1 | 0.5 | Clade tracking |
| vaccine_mismatch_signal | boolean | yes | true/false | true | Mismatch flag |
| antiviral_susceptibility | enum | no | susceptible; reduced; decreased | "susceptible" | Lab results |
| policy_responses | array | no | enum set | ["vaccination_acceleration"] | Interventions |
| anomaly_flags | array | no | enum set | ["mortality_reporting_gap"] | Alerts |
| confidence_level | enum | yes | high; medium; low | "high" | Data reliability |
| city_id | string | yes | — | "AUS_ADELAIDE" | Urban section |
| hospital_capacity_status | enum | yes | normal; strained; critical_overload | "critical_overload" | Capacity |
| reliability_tier | enum | yes | 1; 2; 3 | 1 | Source tier |
| verification_status | enum | yes | verified; pending; refuted | "verified" | Evidence status |

## References

[^1]: World Health Organization. Current influenza update. https://www.who.int/teams/global-influenza-programme/surveillance-and-monitoring/influenza-updates/current-influenza-update

[^2]: U.S. Centers for Disease Control and Prevention. Weekly US Influenza Surveillance Report: Key Updates for Week 47, ending November 22, 2025. https://www.cdc.gov/fluview/surveillance/2025-week-47.html

[^3]: European Centre for Disease Prevention and Control. Threat Assessment Brief: Assessing the risk of influenza for the EU/EEA in the context of increasing circulation of A(H3N2) subclade K (November 2025). https://www.ecdc.europa.eu/en/publications-data/threat-assessment-brief-assessing-risk-influenza-november-2025

[^4]: UK Health Security Agency. National flu and COVID-19 surveillance report: 4 December 2025 (Week 49). https://www.gov.uk/government/statistics/national-flu-and-covid-19-surveillance-reports-2025-to-2026-season/national-flu-and-covid-19-surveillance-report-4-december-2025-week-49

[^5]: The Japan Times. Japan’s flu season comes early as new variant spreads. https://www.japantimes.co.jp/news/2025/12/02/japan/science-health/flu-season/

[^6]: WHO Western Pacific. Respiratory Viruses Surveillance Bulletin: Epidemiological Week 47. https://www.who.int/westernpacific/publications/m/item/respiratory-viruses-surveillance-bulletin--epidemiological-week-47

[^7]: Pan American Health Organization. Epidemiological Alert: Seasonal Influenza in the Americas Region (End of 2025 Season in the Southern Hemisphere; Start of the 2025–26 Season in the Northern Hemisphere). https://www.paho.org/en/documents/epidemiological-alert-seasonal-influenza-americas-region-end-2025-season-southern

[^8]: UK Health Security Agency. Interim effectiveness estimates of 2025–26 influenza vaccines (PDF). https://assets.publishing.service.gov.uk/media/691349457a0ccd6a3aad7fa9/_Flu_interimVE_2526_.pdf

[^9]: U.S. Centers for Disease Control and Prevention. Weekly US Influenza Surveillance Report: Key Updates for Week 46, ending November 15, 2025. https://www.cdc.gov/fluview/surveillance/2025-week-46.html

[^10]: U.S. Centers for Disease Control and Prevention. U.S. Influenza Surveillance: Purpose and Methods | FluView. https://www.cdc.gov/fluview/overview/index.html

[^11]: WHO Western Pacific. Bi-weekly Influenza Situation Update — Virological Surveillance (October 8, 2025). https://cdn.who.int/media/docs/default-source/wpro---documents/emergency/surveillance/seasonal-influenza/influenza_20251008.pdf?sfvrsn=8044af49_1&download=true

[^12]: WHO Western Pacific. Respiratory Viruses Surveillance Bulletin — Epidemiological Week 45, 2025. https://cdn.who.int/media/docs/default-source/wpro---documents/emergency/surveillance/seasonal-influenza/rvsb_epidemiological-week-45--2025.pdf?sfvrsn=acbb71fe_1

[^13]: China CDC Weekly — Current Volume (7). https://weekly.chinacdc.cn/en/zcustom/currentVolume/1

[^14]: U.S. Centers for Disease Control and Prevention. Influenza Activity in the United States during the 2024–25 Season. https://www.cdc.gov/flu/whats-new/2025-2026-influenza-activity.html

[^15]: CIDRAP. With an absent CDC and mismatched 'subclade K' flu strain, experts face upcoming season with uncertainty. https://www.cidrap.umn.edu/influenza-vaccines/absent-cdc-and-mismatched-subclade-k-flu-strain-experts-face-upcoming-season

[^16]: Scientific American. How Bad Will Flu Season Be This Year? https://www.scientificamerican.com/article/how-bad-will-flu-season-be-this-year/

[^17]: Diagnostic Performance of Multiplex Lateral Flow Tests in Ambulatory Patients. https://www.sciencedirect.com/science/article/pii/S0732889324002475

[^18]: Cepheid. Emerging & Evolving Respiratory Viruses: Test Design Matters. https://www.cepheid.com/en-US/insights/insight-hub/respiratory-health/2024/09/emerging-evolving-respiratory-viruses-test-design-matters.html

[^19]: Simultaneous Detection and Differentiation of SARS-CoV-2 and Other Respiratory Viruses (Co-detection challenges). https://pmc.ncbi.nlm.nih.gov/articles/PMC12428728/

[^20]: Viral Co-detection of Influenza Virus and Other Respiratory Viruses (Frontiers in Microbiology, 2024). https://www.frontiersin.org/journals/microbiology/articles/10.3389/fmicb.2024.1462802/full

[^21]: CDC. Respiratory Illnesses Data Channel. https://www.cdc.gov/respiratory-viruses/data/index.html

[^22]: CNN. As respiratory virus season begins, federal shutdown leaves critical gap in surveillance. https://www.cnn.com/2025/10/22/health/disease-surveillance-government-shutdown

[^23]: Governing. Government Shutdown Leaves States Unarmed in Tracking Disease. https://www.governing.com/management-and-administration/government-shutdown-leaves-states-unarmed-in-tracking-disease

[^24]: CBC. Hospital overcrowding makes flu response in northern Ontario more challenging. https://www.cbc.ca/news/canada/sudbury/flu-season-covid-vaccination-9.6986640

[^25]: Australasian College for Emergency Medicine (ACEM). ‘Hidden ramping’ straining South Australia’s emergency departments. https://acem.org.au/News/November-2025/%E2%80%98Hidden-ramping%E2%80%99-straining-South-Australia%E2%80%99s-emerg

[^26]: Adelaide Emergency Departments (Twitter/X). EDs critically overcrowded. https://twitter.com/AdlEmergStatus/status/1996829397198970885

[^27]: ABC News (Australia). ED doctors under pressure at Royal Adelaide Hospital. https://www.abc.net.au/news/2025-11-01/ed-doctors-under-pressure-at-royal-adelaide-hospital/105952042

[^28]: Public Health Ontario. Ontario Respiratory Virus Tool. https://www.publichealthontario.ca/en/About/News/2025/10/October-2025-Enhancements-ORVT

---

Information gaps acknowledged: real-world VE for subclade K is pending; U.S. NCHS mortality data unavailable weeks 39–47; sparse country-level counts in WHO updates; limited official counts for Japan in this context; Canada’s FluWatch+ portal inaccessible; detailed H3N2 counts for China/Australia not extracted; WPRO subtype breakdown limited in week 47.
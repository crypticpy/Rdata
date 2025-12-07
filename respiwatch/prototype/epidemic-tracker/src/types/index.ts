export interface OutbreakData {
  metadata: {
    dataset_name: string;
    version: string;
    last_updated: string;
    data_baseline: string;
    coverage_period: {
      start_date: string;
      end_date: string;
    };
  };
  global_overview: {
    current_status: Record<string, RegionStatus>;
    severity_assessment: {
      global_severity: string;
      vaccine_effectiveness: {
        overall_effectiveness: number;
        h3n2_effectiveness: number;
        subclade_k_effectiveness: number;
      };
    };
    key_anomalies: Anomaly[];
  };
  countries: Record<string, CountryData>;
  timeline: {
    progression_by_date: TimelineEntry[];
    key_epidemiological_events: EpidemiologicalEvent[];
  };
  pathogen_analysis: PathogenAnalysis;
}

export interface RegionStatus {
  status: string;
  positivity_rate: number;
  dominant_pathogen: string;
  confidence_level: string;
  data_sources: string[];
}

export interface Anomaly {
  anomaly_type: string;
  description: string;
  geographic_scope: string;
  confidence_level: string;
  first_detected: string;
}

export interface CountryData {
  country_name: string;
  iso_code: string;
  population: number;
  outbreak_status: string;
  h3n2_data: {
    case_numbers: {
      confirmed_cases?: number;
      estimated_cases?: number;
      hospitalizations?: number;
      deaths?: number;
    };
    positivity_rate: number;
    subclade_k_prevalence: number;
    hospitalization_rate: number;
  };
  healthcare_capacity: {
    icu_bed_utilization: number;
    hospital_capacity_stress: string;
  };
  anomaly_flags: AnomalyFlag[];
  data_confidence_level: string;
  last_updated: string;
  primary_data_source: string;
}

export interface AnomalyFlag {
  anomaly_type: string;
  description: string;
  severity: string;
  first_detected: string;
  verification_status: string;
}

export interface TimelineEntry {
  date: string;
  week_number: number;
  key_events: KeyEvent[];
  surveillance_indicators: {
    positivity_rate: number;
    case_numbers: number;
    hospitalization_rate: number;
  };
}

export interface KeyEvent {
  event_type: string;
  description: string;
  location: string;
  impact_level: string;
}

export interface EpidemiologicalEvent {
  event_name: string;
  event_date: string;
  event_type: string;
  description: string;
  geographic_scope: string;
  impact_assessment: string;
}

export interface PathogenAnalysis {
  primary_pathogen: {
    name: string;
    dominant_strain: string;
    global_prevalence: number;
    severity_assessment: string;
  };
  vaccine_effectiveness: {
    overall_effectiveness: number;
    strain_specific_effectiveness: {
      strain: string;
      effectiveness: number;
    }[];
  };
}

export interface SuspiciousPattern {
  pattern_id: string;
  pattern_type: string;
  pattern_name?: string;
  description: string;
  geographic_scope: string | string[];
  first_detected: string;
  confidence_level: string;
  verification_status: string;
  risk_assessment?: Record<string, string>;
}

export interface AnomalyDetectionFlags {
  metadata: {
    total_anomalies_detected: number;
    critical_anomalies: number;
  };
  suspicious_patterns: SuspiciousPattern[];
  surveillance_gaps: SurveillanceGap[];
}

export interface SurveillanceGap {
  gap_id: string;
  gap_type: string;
  gap_name: string;
  description: string;
  affected_area: string;
  severity: string;
  start_date: string;
  end_date: string;
}

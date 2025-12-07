import { createClient } from '@supabase/supabase-js';

const supabaseUrl = 'https://gbexvvubcuhgdhcpddcv.supabase.co';
const supabaseAnonKey = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImdiZXh2dnViY3VoZ2RoY3BkZGN2Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NjQ5ODcxMjIsImV4cCI6MjA4MDU2MzEyMn0.qNz9SPznMXWoT77yTDPz6zjF6Y5LCanSG0t9lJgpgDI';

export const supabase = createClient(supabaseUrl, supabaseAnonKey);

export interface OutbreakRecord {
  id: string;
  country: string;
  iso_code: string;
  pathogen: string;
  date: string;
  week_number: number | null;
  cases: number;
  hospitalizations: number;
  deaths: number;
  positivity_rate: number | null;
  severity_level: string;
  data_source: string;
  confidence_level: string;
  last_updated: string;
}

export interface Alert {
  id: string;
  alert_type: string;
  severity: string;
  title: string;
  description: string;
  geographic_scope: string;
  source: string;
  first_detected: string;
  verification_status: string;
}

export interface Pathogen {
  id: string;
  name: string;
  display_name: string;
  type: string;
  description: string;
  dominant_strain: string | null;
  global_prevalence: number | null;
  vaccine_effectiveness: number | null;
  is_active: boolean;
}

export async function fetchOutbreakData(pathogen?: string) {
  let query = supabase
    .from('outbreak_data')
    .select('*')
    .order('date', { ascending: false });
  
  if (pathogen) {
    query = query.eq('pathogen', pathogen);
  }
  
  const { data, error } = await query.limit(1000);
  if (error) throw error;
  return data as OutbreakRecord[];
}

export async function fetchAlerts() {
  const { data, error } = await supabase
    .from('alerts')
    .select('*')
    .order('first_detected', { ascending: false });
  
  if (error) throw error;
  return data as Alert[];
}

export async function fetchPathogens() {
  const { data, error } = await supabase
    .from('pathogens')
    .select('*')
    .eq('is_active', true);
  
  if (error) throw error;
  return data as Pathogen[];
}

export interface SourceStatus {
  status: 'success' | 'unavailable';
  records?: number;
  error?: string;
}

export interface RefreshResult {
  message: string;
  sources: Record<string, SourceStatus>;
  timestamp: string;
}

export async function refreshLiveData(): Promise<RefreshResult> {
  const { data, error } = await supabase.functions.invoke('fetch-outbreak-data', {
    body: {}
  });
  
  if (error) throw error;
  return data?.data as RefreshResult;
}

export async function getDataSourceStatus(): Promise<Record<string, SourceStatus>> {
  const { data, error } = await supabase
    .from('data_refresh_log')
    .select('source_status, refreshed_at')
    .order('refreshed_at', { ascending: false })
    .limit(1)
    .single();
  
  if (error || !data?.source_status) {
    return {};
  }
  
  try {
    return JSON.parse(data.source_status);
  } catch {
    return {};
  }
}

export async function getOutbreakByCountry(pathogen?: string) {
  let query = supabase
    .from('outbreak_data')
    .select('*')
    .order('cases', { ascending: false });
  
  if (pathogen) {
    query = query.eq('pathogen', pathogen);
  }
  
  const { data, error } = await query;
  if (error) throw error;
  
  // Group by country, taking latest entry
  const countryMap = new Map<string, OutbreakRecord>();
  for (const record of (data || [])) {
    const existing = countryMap.get(record.iso_code);
    if (!existing || new Date(record.date) > new Date(existing.date)) {
      countryMap.set(record.iso_code, record);
    }
  }
  
  return Array.from(countryMap.values());
}

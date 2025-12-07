-- Migration: setup_rls_and_seed_pathogens
-- Created at: 1764989289

-- Enable RLS on all tables
ALTER TABLE outbreak_data ENABLE ROW LEVEL SECURITY;
ALTER TABLE alerts ENABLE ROW LEVEL SECURITY;
ALTER TABLE pathogens ENABLE ROW LEVEL SECURITY;
ALTER TABLE data_refresh_log ENABLE ROW LEVEL SECURITY;

-- Create policies to allow public read access
CREATE POLICY "Allow public read outbreak_data" ON outbreak_data FOR SELECT USING (true);
CREATE POLICY "Allow public read alerts" ON alerts FOR SELECT USING (true);
CREATE POLICY "Allow public read pathogens" ON pathogens FOR SELECT USING (true);
CREATE POLICY "Allow public read data_refresh_log" ON data_refresh_log FOR SELECT USING (true);

-- Allow edge functions to insert/update
CREATE POLICY "Allow edge function write outbreak_data" ON outbreak_data FOR ALL USING (auth.role() IN ('anon', 'service_role'));
CREATE POLICY "Allow edge function write alerts" ON alerts FOR ALL USING (auth.role() IN ('anon', 'service_role'));
CREATE POLICY "Allow edge function write pathogens" ON pathogens FOR ALL USING (auth.role() IN ('anon', 'service_role'));
CREATE POLICY "Allow edge function write data_refresh_log" ON data_refresh_log FOR ALL USING (auth.role() IN ('anon', 'service_role'));

-- Seed pathogens data
INSERT INTO pathogens (name, display_name, type, description, dominant_strain, global_prevalence, vaccine_effectiveness, is_active) VALUES
('H3N2', 'Influenza A H3N2', 'influenza', 'Seasonal influenza A virus subtype H3N2', 'Subclade K (J.2.4.1)', 73.5, 25.0, true),
('H5N5', 'Avian Influenza H5N5', 'influenza', 'Avian-origin influenza A virus', NULL, 0.1, NULL, true),
('COVID19', 'COVID-19', 'coronavirus', 'SARS-CoV-2 coronavirus disease', 'JN.1', 5.0, 55.0, true),
('RSV', 'Respiratory Syncytial Virus', 'respiratory', 'Respiratory syncytial virus affecting children and elderly', NULL, 12.0, NULL, true),
('MYCOPLASMA', 'Mycoplasma Pneumoniae', 'bacteria', 'Bacterial pathogen causing walking pneumonia', NULL, 8.0, NULL, true);;
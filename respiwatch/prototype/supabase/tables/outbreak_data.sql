CREATE TABLE outbreak_data (
    id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
    country VARCHAR(100) NOT NULL,
    iso_code VARCHAR(10) NOT NULL,
    pathogen VARCHAR(50) NOT NULL,
    date DATE NOT NULL,
    week_number INTEGER,
    cases INTEGER DEFAULT 0,
    hospitalizations INTEGER DEFAULT 0,
    deaths INTEGER DEFAULT 0,
    positivity_rate DECIMAL(5,2),
    severity_level VARCHAR(20),
    data_source VARCHAR(100),
    confidence_level VARCHAR(20),
    last_updated TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
CREATE TABLE pathogens (
    id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
    name VARCHAR(100) NOT NULL UNIQUE,
    display_name VARCHAR(100) NOT NULL,
    type VARCHAR(50),
    description TEXT,
    dominant_strain VARCHAR(100),
    global_prevalence DECIMAL(5,2),
    vaccine_effectiveness DECIMAL(5,2),
    is_active BOOLEAN DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
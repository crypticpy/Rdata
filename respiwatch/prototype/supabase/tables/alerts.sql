CREATE TABLE alerts (
    id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
    alert_type VARCHAR(50) NOT NULL,
    severity VARCHAR(20) NOT NULL,
    title VARCHAR(255) NOT NULL,
    description TEXT,
    geographic_scope VARCHAR(255),
    source VARCHAR(100),
    first_detected DATE,
    verification_status VARCHAR(50) DEFAULT 'unverified',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
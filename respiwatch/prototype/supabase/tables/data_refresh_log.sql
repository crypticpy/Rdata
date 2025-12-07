CREATE TABLE data_refresh_log (
    id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
    source VARCHAR(100) NOT NULL,
    status VARCHAR(20) NOT NULL,
    records_fetched INTEGER DEFAULT 0,
    error_message TEXT,
    refreshed_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
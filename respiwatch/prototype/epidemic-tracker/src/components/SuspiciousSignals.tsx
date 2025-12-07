import { AlertCircle, AlertTriangle, Eye, MapPin } from 'lucide-react';
import type { AnomalyDetectionFlags } from '../types';

interface SuspiciousSignalsProps {
  data: AnomalyDetectionFlags;
  darkMode: boolean;
}

export function SuspiciousSignals({ data, darkMode }: SuspiciousSignalsProps) {
  const patterns = data.suspicious_patterns.slice(0, 5);
  
  const getSeverityConfig = (pattern: typeof patterns[0]) => {
    const riskLevel = pattern.risk_assessment?.healthcare_system_impact || 
                      pattern.risk_assessment?.public_health_impact || 
                      pattern.confidence_level;
    
    if (riskLevel === 'critical' || pattern.pattern_type === 'capacity_crisis') {
      return {
        icon: AlertCircle,
        color: 'text-red-500',
        bg: darkMode ? 'bg-red-500/10' : 'bg-red-50',
        border: darkMode ? 'border-red-500/30' : 'border-red-200',
        label: 'CRITICAL'
      };
    }
    if (riskLevel === 'high' || pattern.pattern_type === 'vaccine_mismatch') {
      return {
        icon: AlertTriangle,
        color: 'text-yellow-500',
        bg: darkMode ? 'bg-yellow-500/10' : 'bg-yellow-50',
        border: darkMode ? 'border-yellow-500/30' : 'border-yellow-200',
        label: 'WARNING'
      };
    }
    return {
      icon: Eye,
      color: 'text-blue-500',
      bg: darkMode ? 'bg-blue-500/10' : 'bg-blue-50',
      border: darkMode ? 'border-blue-500/30' : 'border-blue-200',
      label: 'WATCH'
    };
  };

  return (
    <div className={`rounded-xl ${darkMode ? 'bg-slate-800 border-slate-700' : 'bg-white border-gray-200'} border p-4`}>
      <div className="flex items-center justify-between mb-4">
        <h2 className={`text-lg font-semibold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
          Suspicious Signals
        </h2>
        <span className={`text-xs px-2 py-1 rounded-full ${darkMode ? 'bg-red-500/20 text-red-400' : 'bg-red-100 text-red-600'}`}>
          {data.metadata.total_anomalies_detected} ACTIVE
        </span>
      </div>
      
      <div className="space-y-3 max-h-[400px] overflow-y-auto">
        {patterns.map((pattern, idx) => {
          const config = getSeverityConfig(pattern);
          const Icon = config.icon;
          const location = Array.isArray(pattern.geographic_scope) 
            ? pattern.geographic_scope.join(', ') 
            : pattern.geographic_scope;
          
          return (
            <div 
              key={idx}
              className={`p-3 rounded-lg border ${config.bg} ${config.border}`}
            >
              <div className="flex items-start gap-3">
                <div className={`p-1.5 rounded-lg ${config.bg}`}>
                  <Icon className={`w-4 h-4 ${config.color}`} />
                </div>
                <div className="flex-1 min-w-0">
                  <div className="flex items-center gap-2 mb-1">
                    <span className={`text-xs font-medium ${config.color}`}>
                      {config.label}
                    </span>
                    <span className={`text-xs ${darkMode ? 'text-slate-500' : 'text-gray-400'}`}>
                      {new Date(pattern.first_detected).toLocaleDateString('en-US', { 
                        month: 'short', 
                        day: 'numeric', 
                        year: 'numeric' 
                      })}
                    </span>
                  </div>
                  <h3 className={`text-sm font-medium mb-1 ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                    {pattern.pattern_name || pattern.pattern_type.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase())}
                  </h3>
                  <p className={`text-xs ${darkMode ? 'text-slate-400' : 'text-gray-500'} line-clamp-2`}>
                    {pattern.description}
                  </p>
                  <div className="flex items-center gap-1 mt-2">
                    <MapPin className={`w-3 h-3 ${darkMode ? 'text-slate-500' : 'text-gray-400'}`} />
                    <span className={`text-xs ${darkMode ? 'text-slate-500' : 'text-gray-400'}`}>
                      {location}
                    </span>
                  </div>
                </div>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}

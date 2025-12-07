import { AlertCircle, AlertTriangle, Eye, MapPin } from 'lucide-react';
import type { Alert } from '../lib/supabase';

interface SuspiciousSignalsLiveProps {
  alerts: Alert[];
  darkMode: boolean;
}

export function SuspiciousSignalsLive({ alerts, darkMode }: SuspiciousSignalsLiveProps) {
  const getSeverityConfig = (severity: string) => {
    if (severity === 'critical') {
      return {
        icon: AlertCircle,
        color: 'text-red-500',
        bg: darkMode ? 'bg-red-500/10' : 'bg-red-50',
        border: darkMode ? 'border-red-500/30' : 'border-red-200',
        label: 'CRITICAL'
      };
    }
    if (severity === 'high') {
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
          {alerts.length} ACTIVE
        </span>
      </div>
      
      <div className="space-y-3 max-h-[400px] overflow-y-auto">
        {alerts.slice(0, 5).map((alert) => {
          const config = getSeverityConfig(alert.severity);
          const Icon = config.icon;
          
          return (
            <div 
              key={alert.id}
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
                      {new Date(alert.first_detected).toLocaleDateString('en-US', { 
                        month: 'short', 
                        day: 'numeric', 
                        year: 'numeric' 
                      })}
                    </span>
                  </div>
                  <h3 className={`text-sm font-medium mb-1 ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                    {alert.title}
                  </h3>
                  <p className={`text-xs ${darkMode ? 'text-slate-400' : 'text-gray-500'} line-clamp-2`}>
                    {alert.description}
                  </p>
                  <div className="flex items-center gap-1 mt-2">
                    <MapPin className={`w-3 h-3 ${darkMode ? 'text-slate-500' : 'text-gray-400'}`} />
                    <span className={`text-xs ${darkMode ? 'text-slate-500' : 'text-gray-400'}`}>
                      {alert.geographic_scope}
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

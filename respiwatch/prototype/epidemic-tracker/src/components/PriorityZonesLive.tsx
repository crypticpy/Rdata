import { ChevronRight, TrendingUp, TrendingDown, Minus } from 'lucide-react';
import type { OutbreakRecord } from '../lib/supabase';

interface PriorityZonesLiveProps {
  data: OutbreakRecord[];
  onCountryClick: (iso: string) => void;
  darkMode: boolean;
}

export function PriorityZonesLive({ data, onCountryClick, darkMode }: PriorityZonesLiveProps) {
  const sortedData = [...data]
    .sort((a, b) => (b.cases || 0) - (a.cases || 0))
    .slice(0, 5);

  const getStatusConfig = (severity: string) => {
    switch (severity) {
      case 'critical':
        return { 
          label: 'Epidemic Peak', 
          color: 'text-red-500', 
          bg: darkMode ? 'bg-red-500/10' : 'bg-red-50',
          icon: TrendingUp
        };
      case 'high':
        return { 
          label: 'Rising', 
          color: 'text-orange-500', 
          bg: darkMode ? 'bg-orange-500/10' : 'bg-orange-50',
          icon: TrendingUp
        };
      case 'moderate':
        return { 
          label: 'Elevated', 
          color: 'text-yellow-500', 
          bg: darkMode ? 'bg-yellow-500/10' : 'bg-yellow-50',
          icon: Minus
        };
      default:
        return { 
          label: 'Monitoring', 
          color: 'text-blue-500', 
          bg: darkMode ? 'bg-blue-500/10' : 'bg-blue-50',
          icon: TrendingDown
        };
    }
  };

  return (
    <div className={`rounded-xl ${darkMode ? 'bg-slate-800 border-slate-700' : 'bg-white border-gray-200'} border p-4`}>
      <div className="flex items-center justify-between mb-4">
        <h2 className={`text-lg font-semibold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
          Priority Monitoring Zones
        </h2>
        <span className={`text-xs ${darkMode ? 'text-blue-400' : 'text-blue-600'}`}>
          Live Data
        </span>
      </div>

      <div className="space-y-2">
        {sortedData.map((record) => {
          const statusConfig = getStatusConfig(record.severity_level);
          const StatusIcon = statusConfig.icon;
          
          return (
            <button
              key={record.id}
              onClick={() => onCountryClick(record.iso_code)}
              className={`w-full p-3 rounded-lg ${darkMode ? 'hover:bg-slate-700' : 'hover:bg-gray-50'} transition-colors text-left group`}
            >
              <div className="flex items-center justify-between">
                <div className="flex items-center gap-3">
                  <div className={`w-2 h-2 rounded-full ${
                    record.severity_level === 'critical' ? 'bg-red-500' :
                    record.severity_level === 'high' ? 'bg-orange-500' :
                    record.severity_level === 'moderate' ? 'bg-yellow-500' :
                    'bg-blue-500'
                  }`} />
                  <div>
                    <h3 className={`font-medium ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                      {record.country}
                    </h3>
                    <div className="flex items-center gap-2 mt-0.5">
                      <span className={`text-xs px-1.5 py-0.5 rounded ${statusConfig.bg} ${statusConfig.color}`}>
                        <StatusIcon className="w-3 h-3 inline mr-1" />
                        {statusConfig.label}
                      </span>
                      <span className={`text-xs ${darkMode ? 'text-slate-500' : 'text-gray-400'}`}>
                        {record.pathogen}
                      </span>
                    </div>
                  </div>
                </div>
                <div className="flex items-center gap-2">
                  <div className="text-right">
                    <p className={`text-sm font-semibold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                      {record.positivity_rate ? `${record.positivity_rate}% Positivity` : 'N/A'}
                    </p>
                    <p className={`text-xs ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
                      {(record.cases || 0).toLocaleString()} cases
                    </p>
                  </div>
                  <ChevronRight className={`w-4 h-4 ${darkMode ? 'text-slate-500' : 'text-gray-400'} group-hover:translate-x-1 transition-transform`} />
                </div>
              </div>
            </button>
          );
        })}
      </div>
    </div>
  );
}

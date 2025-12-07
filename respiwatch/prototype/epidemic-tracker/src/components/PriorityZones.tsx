import { ChevronRight, TrendingUp, TrendingDown, Minus } from 'lucide-react';
import type { OutbreakData } from '../types';

interface PriorityZonesProps {
  data: OutbreakData;
  onCountryClick: (iso: string) => void;
  darkMode: boolean;
}

export function PriorityZones({ data, onCountryClick, darkMode }: PriorityZonesProps) {
  const countries = Object.entries(data.countries)
    .filter(([iso]) => iso !== 'EEA')
    .map(([iso, country]) => ({
      iso,
      ...country,
      priority: country.h3n2_data.positivity_rate * (country.outbreak_status === 'peak' ? 2 : 1)
    }))
    .sort((a, b) => b.priority - a.priority)
    .slice(0, 5);

  const getStatusConfig = (status: string) => {
    switch (status) {
      case 'peak':
        return { 
          label: 'Epidemic Peak', 
          color: 'text-red-500', 
          bg: darkMode ? 'bg-red-500/10' : 'bg-red-50',
          icon: TrendingUp
        };
      case 'active_outbreak':
        return { 
          label: 'Rising', 
          color: 'text-orange-500', 
          bg: darkMode ? 'bg-orange-500/10' : 'bg-orange-50',
          icon: TrendingUp
        };
      case 'elevated_activity':
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
        <button className={`text-sm ${darkMode ? 'text-blue-400 hover:text-blue-300' : 'text-blue-600 hover:text-blue-700'}`}>
          View Full Report
        </button>
      </div>

      <div className="space-y-2">
        {countries.map((country) => {
          const statusConfig = getStatusConfig(country.outbreak_status);
          const StatusIcon = statusConfig.icon;
          
          return (
            <button
              key={country.iso}
              onClick={() => onCountryClick(country.iso)}
              className={`w-full p-3 rounded-lg ${darkMode ? 'hover:bg-slate-700' : 'hover:bg-gray-50'} transition-colors text-left group`}
            >
              <div className="flex items-center justify-between">
                <div className="flex items-center gap-3">
                  <div className={`w-2 h-2 rounded-full ${
                    country.outbreak_status === 'peak' ? 'bg-red-500' :
                    country.outbreak_status === 'active_outbreak' ? 'bg-orange-500' :
                    country.outbreak_status === 'elevated_activity' ? 'bg-yellow-500' :
                    'bg-blue-500'
                  }`} />
                  <div>
                    <h3 className={`font-medium ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                      {country.country_name}
                    </h3>
                    <div className="flex items-center gap-2 mt-0.5">
                      <span className={`text-xs px-1.5 py-0.5 rounded ${statusConfig.bg} ${statusConfig.color}`}>
                        <StatusIcon className="w-3 h-3 inline mr-1" />
                        {statusConfig.label}
                      </span>
                    </div>
                  </div>
                </div>
                <div className="flex items-center gap-2">
                  <div className="text-right">
                    <p className={`text-sm font-semibold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                      {country.h3n2_data.positivity_rate > 30 ? 'High' : 
                       country.h3n2_data.positivity_rate > 15 ? 'Moderate' : 'Low'} ({country.h3n2_data.positivity_rate}% Positivity)
                    </p>
                    <p className={`text-xs ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
                      {country.h3n2_data.case_numbers.estimated_cases?.toLocaleString() || 
                       country.h3n2_data.case_numbers.confirmed_cases?.toLocaleString() || 'N/A'} cases
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

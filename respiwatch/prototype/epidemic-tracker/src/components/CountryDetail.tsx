import { X, AlertTriangle, Activity, Users, Building2 } from 'lucide-react';
import type { CountryData } from '../types';

interface CountryDetailProps {
  country: CountryData;
  onClose: () => void;
  darkMode: boolean;
}

export function CountryDetail({ country, onClose, darkMode }: CountryDetailProps) {
  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/50 backdrop-blur-sm">
      <div className={`w-full max-w-2xl rounded-xl ${darkMode ? 'bg-slate-800' : 'bg-white'} shadow-2xl`}>
        {/* Header */}
        <div className={`flex items-center justify-between p-4 border-b ${darkMode ? 'border-slate-700' : 'border-gray-200'}`}>
          <div>
            <h2 className={`text-xl font-bold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
              {country.country_name}
            </h2>
            <p className={`text-sm ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
              {country.primary_data_source} - Updated {new Date(country.last_updated).toLocaleDateString()}
            </p>
          </div>
          <button
            onClick={onClose}
            className={`p-2 rounded-lg ${darkMode ? 'hover:bg-slate-700' : 'hover:bg-gray-100'}`}
          >
            <X className={`w-5 h-5 ${darkMode ? 'text-slate-400' : 'text-gray-500'}`} />
          </button>
        </div>

        {/* Content */}
        <div className="p-4 space-y-4">
          {/* Status Badge */}
          <div className="flex items-center gap-2">
            <span className={`px-3 py-1 rounded-full text-sm font-medium ${
              country.outbreak_status === 'peak' ? 'bg-red-500/20 text-red-500' :
              country.outbreak_status === 'active_outbreak' ? 'bg-orange-500/20 text-orange-500' :
              country.outbreak_status === 'elevated_activity' ? 'bg-yellow-500/20 text-yellow-500' :
              'bg-blue-500/20 text-blue-500'
            }`}>
              {country.outbreak_status.replace(/_/g, ' ').toUpperCase()}
            </span>
            <span className={`text-sm ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
              Confidence: {country.data_confidence_level}
            </span>
          </div>

          {/* Stats Grid */}
          <div className="grid grid-cols-2 gap-4">
            <div className={`p-4 rounded-lg ${darkMode ? 'bg-slate-700/50' : 'bg-gray-50'}`}>
              <div className="flex items-center gap-2 mb-2">
                <Activity className={`w-4 h-4 ${darkMode ? 'text-blue-400' : 'text-blue-600'}`} />
                <span className={`text-sm font-medium ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                  Positivity Rate
                </span>
              </div>
              <p className={`text-2xl font-bold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                {country.h3n2_data.positivity_rate}%
              </p>
            </div>
            <div className={`p-4 rounded-lg ${darkMode ? 'bg-slate-700/50' : 'bg-gray-50'}`}>
              <div className="flex items-center gap-2 mb-2">
                <Users className={`w-4 h-4 ${darkMode ? 'text-orange-400' : 'text-orange-600'}`} />
                <span className={`text-sm font-medium ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                  Subclade K Prevalence
                </span>
              </div>
              <p className={`text-2xl font-bold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                {country.h3n2_data.subclade_k_prevalence}%
              </p>
            </div>
            <div className={`p-4 rounded-lg ${darkMode ? 'bg-slate-700/50' : 'bg-gray-50'}`}>
              <div className="flex items-center gap-2 mb-2">
                <Building2 className={`w-4 h-4 ${darkMode ? 'text-red-400' : 'text-red-600'}`} />
                <span className={`text-sm font-medium ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                  ICU Utilization
                </span>
              </div>
              <p className={`text-2xl font-bold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                {country.healthcare_capacity.icu_bed_utilization}%
              </p>
            </div>
            <div className={`p-4 rounded-lg ${darkMode ? 'bg-slate-700/50' : 'bg-gray-50'}`}>
              <div className="flex items-center gap-2 mb-2">
                <Activity className={`w-4 h-4 ${darkMode ? 'text-green-400' : 'text-green-600'}`} />
                <span className={`text-sm font-medium ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                  Estimated Cases
                </span>
              </div>
              <p className={`text-2xl font-bold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                {(country.h3n2_data.case_numbers.estimated_cases || country.h3n2_data.case_numbers.confirmed_cases || 0).toLocaleString()}
              </p>
            </div>
          </div>

          {/* Anomaly Flags */}
          {country.anomaly_flags && country.anomaly_flags.length > 0 && (
            <div>
              <h3 className={`text-sm font-medium mb-2 ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                Active Alerts
              </h3>
              <div className="space-y-2">
                {country.anomaly_flags.map((flag, idx) => (
                  <div 
                    key={idx}
                    className={`p-3 rounded-lg flex items-start gap-2 ${
                      flag.severity === 'critical' ? 'bg-red-500/10' :
                      flag.severity === 'high' ? 'bg-orange-500/10' :
                      'bg-yellow-500/10'
                    }`}
                  >
                    <AlertTriangle className={`w-4 h-4 flex-shrink-0 mt-0.5 ${
                      flag.severity === 'critical' ? 'text-red-500' :
                      flag.severity === 'high' ? 'text-orange-500' :
                      'text-yellow-500'
                    }`} />
                    <div>
                      <p className={`text-sm font-medium ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                        {flag.anomaly_type.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase())}
                      </p>
                      <p className={`text-xs ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
                        {flag.description}
                      </p>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          )}
        </div>

        {/* Footer */}
        <div className={`p-4 border-t ${darkMode ? 'border-slate-700' : 'border-gray-200'}`}>
          <button
            onClick={onClose}
            className="w-full py-2 rounded-lg bg-blue-600 hover:bg-blue-500 text-white font-medium transition-colors"
          >
            Close
          </button>
        </div>
      </div>
    </div>
  );
}

import { X, Activity, Users, Building2, Calendar } from 'lucide-react';
import type { OutbreakRecord } from '../lib/supabase';

interface CountryDetailLiveProps {
  countryCode: string;
  data: OutbreakRecord[];
  onClose: () => void;
  darkMode: boolean;
}

export function CountryDetailLive({ countryCode, data, onClose, darkMode }: CountryDetailLiveProps) {
  const countryRecords = data.filter(d => d.iso_code === countryCode);
  const primaryRecord = countryRecords[0];
  
  if (!primaryRecord) {
    return null;
  }

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/50 backdrop-blur-sm">
      <div className={`w-full max-w-2xl rounded-xl ${darkMode ? 'bg-slate-800' : 'bg-white'} shadow-2xl`}>
        {/* Header */}
        <div className={`flex items-center justify-between p-4 border-b ${darkMode ? 'border-slate-700' : 'border-gray-200'}`}>
          <div>
            <h2 className={`text-xl font-bold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
              {primaryRecord.country}
            </h2>
            <p className={`text-sm ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
              {primaryRecord.data_source} - Updated {new Date(primaryRecord.last_updated).toLocaleDateString()}
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
              primaryRecord.severity_level === 'critical' ? 'bg-red-500/20 text-red-500' :
              primaryRecord.severity_level === 'high' ? 'bg-orange-500/20 text-orange-500' :
              primaryRecord.severity_level === 'moderate' ? 'bg-yellow-500/20 text-yellow-500' :
              'bg-blue-500/20 text-blue-500'
            }`}>
              {primaryRecord.severity_level?.toUpperCase()}
            </span>
            <span className={`text-sm ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
              Confidence: {primaryRecord.confidence_level}
            </span>
          </div>

          {/* Pathogens in this country */}
          <div>
            <h3 className={`text-sm font-medium mb-2 ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
              Active Pathogens
            </h3>
            <div className="flex flex-wrap gap-2">
              {countryRecords.map((record, idx) => (
                <span 
                  key={idx}
                  className={`px-2 py-1 rounded text-xs font-medium ${darkMode ? 'bg-slate-700 text-slate-300' : 'bg-gray-100 text-gray-600'}`}
                >
                  {record.pathogen}: {record.cases?.toLocaleString()} cases
                </span>
              ))}
            </div>
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
                {primaryRecord.positivity_rate || 'N/A'}%
              </p>
            </div>
            <div className={`p-4 rounded-lg ${darkMode ? 'bg-slate-700/50' : 'bg-gray-50'}`}>
              <div className="flex items-center gap-2 mb-2">
                <Users className={`w-4 h-4 ${darkMode ? 'text-orange-400' : 'text-orange-600'}`} />
                <span className={`text-sm font-medium ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                  Total Cases
                </span>
              </div>
              <p className={`text-2xl font-bold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                {countryRecords.reduce((sum, r) => sum + (r.cases || 0), 0).toLocaleString()}
              </p>
            </div>
            <div className={`p-4 rounded-lg ${darkMode ? 'bg-slate-700/50' : 'bg-gray-50'}`}>
              <div className="flex items-center gap-2 mb-2">
                <Building2 className={`w-4 h-4 ${darkMode ? 'text-red-400' : 'text-red-600'}`} />
                <span className={`text-sm font-medium ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                  Hospitalizations
                </span>
              </div>
              <p className={`text-2xl font-bold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                {countryRecords.reduce((sum, r) => sum + (r.hospitalizations || 0), 0).toLocaleString()}
              </p>
            </div>
            <div className={`p-4 rounded-lg ${darkMode ? 'bg-slate-700/50' : 'bg-gray-50'}`}>
              <div className="flex items-center gap-2 mb-2">
                <Calendar className={`w-4 h-4 ${darkMode ? 'text-green-400' : 'text-green-600'}`} />
                <span className={`text-sm font-medium ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                  Week Number
                </span>
              </div>
              <p className={`text-2xl font-bold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                W{primaryRecord.week_number || 'N/A'}
              </p>
            </div>
          </div>
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

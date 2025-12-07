import { CheckCircle, XCircle, AlertCircle, Wifi, Calculator } from 'lucide-react';
import type { SourceStatus } from '../lib/supabase';

interface DataSourceStatusProps {
  sources: Record<string, SourceStatus>;
  darkMode: boolean;
}

const SOURCE_LABELS: Record<string, { name: string; type: 'LIVE' | 'MODELED' }> = {
  fluview_us: { name: 'CDC FluView (US)', type: 'LIVE' },
  owid_covid: { name: 'Our World in Data', type: 'LIVE' },
  global_h3n2: { name: 'WHO GISRS Model', type: 'MODELED' },
  h5n1: { name: 'H5N1 Surveillance', type: 'MODELED' },
  mycoplasma: { name: 'Mycoplasma Model', type: 'MODELED' },
  rsv: { name: 'RSV Model', type: 'MODELED' },
  covid19: { name: 'COVID-19 Global', type: 'LIVE' },
  owid: { name: 'Our World in Data', type: 'LIVE' },
};

export function DataSourceStatus({ sources, darkMode }: DataSourceStatusProps) {
  const entries = Object.entries(sources).filter(([key]) => 
    key !== 'dynamic_alerts' && SOURCE_LABELS[key]
  );
  
  if (entries.length === 0) {
    return null;
  }

  const liveCount = entries.filter(([key, s]) => 
    SOURCE_LABELS[key]?.type === 'LIVE' && s.status === 'success'
  ).length;
  const modeledCount = entries.filter(([key, s]) => 
    SOURCE_LABELS[key]?.type === 'MODELED' && s.status === 'success'
  ).length;

  return (
    <div className={`rounded-lg p-4 ${darkMode ? 'bg-slate-800 border-slate-700' : 'bg-white border-gray-200'} border`}>
      <div className="flex items-center justify-between mb-3">
        <h3 className={`text-sm font-semibold ${darkMode ? 'text-slate-200' : 'text-gray-800'}`}>
          Data Source Transparency
        </h3>
        <div className="flex gap-2">
          <span className="text-xs px-2 py-1 rounded-full bg-green-500/20 text-green-400 flex items-center gap-1">
            <Wifi className="w-3 h-3" />
            {liveCount} Live
          </span>
          <span className="text-xs px-2 py-1 rounded-full bg-blue-500/20 text-blue-400 flex items-center gap-1">
            <Calculator className="w-3 h-3" />
            {modeledCount} Modeled
          </span>
        </div>
      </div>
      
      {/* Live Data Sources */}
      <div className="mb-3">
        <div className={`text-xs font-medium mb-2 flex items-center gap-1 ${darkMode ? 'text-green-400' : 'text-green-600'}`}>
          <Wifi className="w-3 h-3" /> Live API Data
        </div>
        <div className="grid grid-cols-2 md:grid-cols-3 gap-2">
          {entries
            .filter(([key]) => SOURCE_LABELS[key]?.type === 'LIVE')
            .map(([key, source]) => (
              <div 
                key={key}
                className={`flex items-center gap-2 px-2 py-1.5 rounded text-xs ${
                  darkMode ? 'bg-green-900/20 border border-green-800/30' : 'bg-green-50 border border-green-200'
                }`}
              >
                {source.status === 'success' ? (
                  <CheckCircle className="w-3.5 h-3.5 text-green-500 flex-shrink-0" />
                ) : (
                  <XCircle className="w-3.5 h-3.5 text-red-500 flex-shrink-0" />
                )}
                <span className={`truncate ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                  {SOURCE_LABELS[key]?.name || key}
                </span>
                {source.status === 'success' && source.records !== undefined && (
                  <span className={`ml-auto text-xs ${darkMode ? 'text-green-400' : 'text-green-600'}`}>
                    {source.records}
                  </span>
                )}
              </div>
          ))}
        </div>
      </div>

      {/* Modeled Data Sources */}
      <div>
        <div className={`text-xs font-medium mb-2 flex items-center gap-1 ${darkMode ? 'text-blue-400' : 'text-blue-600'}`}>
          <Calculator className="w-3 h-3" /> Epidemiological Models (Estimated)
        </div>
        <div className="grid grid-cols-2 md:grid-cols-3 gap-2">
          {entries
            .filter(([key]) => SOURCE_LABELS[key]?.type === 'MODELED')
            .map(([key, source]) => (
              <div 
                key={key}
                className={`flex items-center gap-2 px-2 py-1.5 rounded text-xs ${
                  darkMode ? 'bg-blue-900/20 border border-blue-800/30' : 'bg-blue-50 border border-blue-200'
                }`}
              >
                {source.status === 'success' ? (
                  <CheckCircle className="w-3.5 h-3.5 text-blue-500 flex-shrink-0" />
                ) : (
                  <XCircle className="w-3.5 h-3.5 text-red-500 flex-shrink-0" />
                )}
                <span className={`truncate ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                  {SOURCE_LABELS[key]?.name || key}
                </span>
                {source.status === 'success' && source.records !== undefined && (
                  <span className={`ml-auto text-xs ${darkMode ? 'text-blue-400' : 'text-blue-600'}`}>
                    {source.records}
                  </span>
                )}
              </div>
          ))}
        </div>
      </div>
      
      {/* Transparency Note */}
      <div className={`mt-3 flex items-start gap-2 text-xs ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
        <AlertCircle className="w-3.5 h-3.5 mt-0.5 flex-shrink-0 text-yellow-500" />
        <span>
          <strong>Live:</strong> Real-time data from CDC/OWID APIs. <strong>Modeled:</strong> Epidemiological estimates based on WHO/CDC surveillance patterns.
        </span>
      </div>
    </div>
  );
}

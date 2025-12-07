import { FileText, AlertTriangle } from 'lucide-react';
import type { Alert, Pathogen } from '../lib/supabase';

interface AnalystNotesLiveProps {
  alerts: Alert[];
  pathogens: Pathogen[];
  selectedPathogen: string | null;
  darkMode: boolean;
}

export function AnalystNotesLive({ alerts, pathogens, selectedPathogen, darkMode }: AnalystNotesLiveProps) {
  const selectedPathogenInfo = pathogens.find(p => p.name === selectedPathogen);
  
  const vaccineAlert = alerts.find(a => a.alert_type === 'vaccine_mismatch');
  const novelAlert = alerts.find(a => a.alert_type === 'novel_pathogen');

  const notes = [
    {
      title: 'Pattern Recognition',
      content: selectedPathogenInfo 
        ? `Current analysis focuses on ${selectedPathogenInfo.display_name}. ${selectedPathogenInfo.dominant_strain ? `Dominant strain: ${selectedPathogenInfo.dominant_strain}.` : ''} ${selectedPathogenInfo.vaccine_effectiveness ? `Vaccine effectiveness estimated at ${selectedPathogenInfo.vaccine_effectiveness}%.` : ''}`
        : 'The current H3N2 wave is following a "Southern-Late" to "Northern-Early" transfer pattern. The mismatch in the vaccine formulation (Subclade K) is the primary driver.',
      type: 'analysis'
    },
    ...(vaccineAlert ? [{
      title: 'Vaccine Alert',
      content: vaccineAlert.description || 'Significant antigenic divergence detected from current vaccine strain.',
      type: 'warning'
    }] : []),
    ...(novelAlert ? [{
      title: 'Novel Threat',
      content: novelAlert.description || 'Novel pathogen detected requiring monitoring.',
      type: 'warning'
    }] : [])
  ];

  return (
    <div className={`rounded-xl ${darkMode ? 'bg-slate-800 border-slate-700' : 'bg-white border-gray-200'} border p-4`}>
      <div className="flex items-center gap-2 mb-4">
        <FileText className={`w-5 h-5 ${darkMode ? 'text-slate-400' : 'text-gray-500'}`} />
        <h2 className={`text-lg font-semibold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
          Analyst Notes
        </h2>
      </div>

      <div className="space-y-4">
        {notes.map((note, idx) => (
          <div key={idx} className={`p-3 rounded-lg ${darkMode ? 'bg-slate-700/50' : 'bg-gray-50'}`}>
            <div className="flex items-center gap-2 mb-2">
              {note.type === 'warning' && (
                <span className="text-xs font-medium text-red-500 flex items-center gap-1">
                  <AlertTriangle className="w-3 h-3" />
                  {note.title}:
                </span>
              )}
              {note.type === 'analysis' && (
                <span className={`text-xs font-medium ${darkMode ? 'text-blue-400' : 'text-blue-600'}`}>
                  {note.title}:
                </span>
              )}
            </div>
            <p className={`text-sm ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
              {note.content}
            </p>
          </div>
        ))}
      </div>

      <div className={`mt-4 pt-4 border-t ${darkMode ? 'border-slate-700' : 'border-gray-200'}`}>
        <p className={`text-xs ${darkMode ? 'text-slate-500' : 'text-gray-400'}`}>
          Data sources: WHO FluMart, CDC, ECDC, UKHSA, Our World in Data
        </p>
      </div>
    </div>
  );
}

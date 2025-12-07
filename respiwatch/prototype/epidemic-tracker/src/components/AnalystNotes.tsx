import { FileText, AlertTriangle } from 'lucide-react';
import type { OutbreakData, AnomalyDetectionFlags } from '../types';

interface AnalystNotesProps {
  data: OutbreakData;
  anomalyData: AnomalyDetectionFlags;
  darkMode: boolean;
}

export function AnalystNotes({ data, anomalyData, darkMode }: AnalystNotesProps) {
  const vaccineInfo = data.global_overview.severity_assessment.vaccine_effectiveness;
  
  const notes = [
    {
      title: 'Pattern Recognition',
      content: `The current H3N2 wave is following a "Southern-Late" to "Northern-Early" transfer pattern. The mismatch in the vaccine formulation (Subclade K) is the primary driver with only ${vaccineInfo.subclade_k_effectiveness}% effectiveness.`,
      type: 'analysis'
    },
    {
      title: 'Critical Watch',
      content: 'The H5N5 case in Washington is currently isolated, but the co-circulation of seasonal flu increases the theoretical risk of reassortment.',
      type: 'warning'
    }
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
                  Critical Watch:
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
          Automated detection via HealthMap & ProMED scraping
        </p>
      </div>
    </div>
  );
}

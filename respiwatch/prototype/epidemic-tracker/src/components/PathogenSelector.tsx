import type { Pathogen } from '../lib/supabase';

interface PathogenSelectorProps {
  pathogens: Pathogen[];
  selectedPathogen: string | null;
  onSelect: (pathogen: string | null) => void;
  darkMode: boolean;
}

const pathogenColors: Record<string, { bg: string; border: string; text: string }> = {
  H3N2: { bg: 'bg-orange-500/20', border: 'border-orange-500', text: 'text-orange-500' },
  H5N5: { bg: 'bg-red-500/20', border: 'border-red-500', text: 'text-red-500' },
  COVID19: { bg: 'bg-purple-500/20', border: 'border-purple-500', text: 'text-purple-500' },
  RSV: { bg: 'bg-blue-500/20', border: 'border-blue-500', text: 'text-blue-500' },
  MYCOPLASMA: { bg: 'bg-green-500/20', border: 'border-green-500', text: 'text-green-500' }
};

export function PathogenSelector({ pathogens, selectedPathogen, onSelect, darkMode }: PathogenSelectorProps) {
  return (
    <div className={`rounded-xl ${darkMode ? 'bg-slate-800 border-slate-700' : 'bg-white border-gray-200'} border p-4 mb-4`}>
      <div className="flex items-center justify-between mb-3">
        <h3 className={`text-sm font-medium ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
          PATHOGEN LAYER CONTROLS
        </h3>
        <button
          onClick={() => onSelect(null)}
          className={`text-xs px-2 py-1 rounded ${
            selectedPathogen === null
              ? 'bg-blue-600 text-white'
              : darkMode ? 'bg-slate-700 text-slate-300' : 'bg-gray-100 text-gray-600'
          }`}
        >
          Show All
        </button>
      </div>
      <div className="flex flex-wrap gap-2">
        {pathogens.map((pathogen) => {
          const colors = pathogenColors[pathogen.name] || { bg: 'bg-gray-500/20', border: 'border-gray-500', text: 'text-gray-500' };
          const isSelected = selectedPathogen === pathogen.name;
          
          return (
            <button
              key={pathogen.id}
              onClick={() => onSelect(isSelected ? null : pathogen.name)}
              className={`px-3 py-2 rounded-lg border-2 transition-all flex items-center gap-2 ${
                isSelected 
                  ? `${colors.bg} ${colors.border}` 
                  : darkMode 
                    ? 'bg-slate-700/50 border-slate-600 hover:border-slate-500' 
                    : 'bg-gray-50 border-gray-200 hover:border-gray-300'
              }`}
            >
              <span className={`w-2 h-2 rounded-full ${isSelected ? colors.text.replace('text-', 'bg-') : darkMode ? 'bg-slate-400' : 'bg-gray-400'}`}></span>
              <span className={`text-sm font-medium ${isSelected ? colors.text : darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
                {pathogen.display_name}
              </span>
              {pathogen.global_prevalence && (
                <span className={`text-xs ${darkMode ? 'text-slate-500' : 'text-gray-400'}`}>
                  {pathogen.global_prevalence}%
                </span>
              )}
            </button>
          );
        })}
      </div>
      {selectedPathogen && (
        <div className={`mt-3 pt-3 border-t ${darkMode ? 'border-slate-700' : 'border-gray-200'}`}>
          <div className="flex items-center gap-4 text-xs">
            {pathogens.find(p => p.name === selectedPathogen)?.dominant_strain && (
              <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>
                Dominant Strain: <strong>{pathogens.find(p => p.name === selectedPathogen)?.dominant_strain}</strong>
              </span>
            )}
            {pathogens.find(p => p.name === selectedPathogen)?.vaccine_effectiveness && (
              <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>
                Vaccine Effectiveness: <strong>{pathogens.find(p => p.name === selectedPathogen)?.vaccine_effectiveness}%</strong>
              </span>
            )}
          </div>
        </div>
      )}
    </div>
  );
}

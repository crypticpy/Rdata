import { Play, Pause, SkipBack, SkipForward } from 'lucide-react';

interface TimelineControlProps {
  selectedWeek: number;
  onWeekChange: (week: number) => void;
  isPlaying: boolean;
  onPlayPause: () => void;
  darkMode: boolean;
}

export function TimelineControl({ 
  selectedWeek, 
  onWeekChange, 
  isPlaying, 
  onPlayPause,
  darkMode 
}: TimelineControlProps) {
  const minWeek = 40;
  const maxWeek = 49;
  
  const getWeekDate = (week: number) => {
    const startDate = new Date('2025-09-30');
    startDate.setDate(startDate.getDate() + (week - 40) * 7);
    return startDate.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
  };

  return (
    <div className={`mt-4 p-4 rounded-lg ${darkMode ? 'bg-slate-700/50' : 'bg-gray-100'}`}>
      <div className="flex items-center justify-between mb-2">
        <span className={`text-xs font-medium ${darkMode ? 'text-slate-300' : 'text-gray-600'}`}>
          TIME-LAPSE CONTROL
        </span>
        <span className={`text-sm ${darkMode ? 'text-slate-300' : 'text-gray-700'}`}>
          Week {selectedWeek} - {getWeekDate(selectedWeek)}, 2025
        </span>
      </div>
      
      {/* Timeline Slider */}
      <div className="relative mt-3 mb-2">
        <input
          type="range"
          min={minWeek}
          max={maxWeek}
          value={selectedWeek}
          onChange={(e) => onWeekChange(parseInt(e.target.value))}
          className="w-full h-3 rounded-lg cursor-pointer accent-blue-500"
          style={{
            background: darkMode 
              ? `linear-gradient(to right, #3b82f6 0%, #3b82f6 ${((selectedWeek - minWeek) / (maxWeek - minWeek)) * 100}%, #475569 ${((selectedWeek - minWeek) / (maxWeek - minWeek)) * 100}%, #475569 100%)`
              : `linear-gradient(to right, #3b82f6 0%, #3b82f6 ${((selectedWeek - minWeek) / (maxWeek - minWeek)) * 100}%, #d1d5db ${((selectedWeek - minWeek) / (maxWeek - minWeek)) * 100}%, #d1d5db 100%)`
          }}
        />
        <div className="flex justify-between mt-1">
          <span className={`text-xs ${darkMode ? 'text-slate-500' : 'text-gray-400'}`}>
            Sep 30, 2025
          </span>
          <span className={`text-xs ${darkMode ? 'text-slate-500' : 'text-gray-400'}`}>
            Dec 12, 2025
          </span>
        </div>
      </div>

      {/* Playback Controls */}
      <div className="flex items-center justify-center gap-4 mt-4">
        <button
          onClick={() => onWeekChange(Math.max(minWeek, selectedWeek - 1))}
          className={`p-2 rounded-lg ${darkMode ? 'hover:bg-slate-600' : 'hover:bg-gray-200'} transition-colors`}
        >
          <SkipBack className={`w-5 h-5 ${darkMode ? 'text-slate-300' : 'text-gray-600'}`} />
        </button>
        <button
          onClick={onPlayPause}
          className={`p-3 rounded-full ${darkMode ? 'bg-blue-600 hover:bg-blue-500' : 'bg-blue-500 hover:bg-blue-600'} transition-colors`}
        >
          {isPlaying ? (
            <Pause className="w-5 h-5 text-white" />
          ) : (
            <Play className="w-5 h-5 text-white" />
          )}
        </button>
        <button
          onClick={() => onWeekChange(Math.min(maxWeek, selectedWeek + 1))}
          className={`p-2 rounded-lg ${darkMode ? 'hover:bg-slate-600' : 'hover:bg-gray-200'} transition-colors`}
        >
          <SkipForward className={`w-5 h-5 ${darkMode ? 'text-slate-300' : 'text-gray-600'}`} />
        </button>
        <span className={`text-sm ml-4 ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
          {selectedWeek - minWeek + 1} / {maxWeek - minWeek + 1}
        </span>
      </div>
    </div>
  );
}

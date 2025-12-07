import { useState } from 'react';
import type { OutbreakRecord } from '../lib/supabase';

interface OutbreakChartLiveProps {
  data: OutbreakRecord[];
  selectedPathogen: string | null;
  darkMode: boolean;
}

export function OutbreakChartLive({ data, selectedPathogen, darkMode }: OutbreakChartLiveProps) {
  const [chartMode, setChartMode] = useState<'absolute' | 'growth'>('absolute');
  
  // Group data by week
  const weeklyData = data.reduce((acc, record) => {
    const week = record.week_number || 48;
    if (!acc[week]) {
      acc[week] = { week, total: 0, usa: 0, china: 0, eu: 0 };
    }
    acc[week].total += record.cases || 0;
    if (record.iso_code === 'USA') acc[week].usa += record.cases || 0;
    if (record.iso_code === 'CHN') acc[week].china += record.cases || 0;
    if (['DEU', 'FRA', 'ITA', 'ESP', 'GBR'].includes(record.iso_code)) {
      acc[week].eu += record.cases || 0;
    }
    return acc;
  }, {} as Record<number, { week: number; total: number; usa: number; china: number; eu: number }>);

  const chartData = Object.values(weeklyData).sort((a, b) => a.week - b.week);
  
  // Add weeks 40-48 if missing
  const allWeeks = [];
  for (let w = 40; w <= 49; w++) {
    allWeeks.push(chartData.find(d => d.week === w) || { week: w, total: 0, usa: 0, china: 0, eu: 0 });
  }

  const maxValue = Math.max(...allWeeks.map(d => d.total), 1);

  const formatValue = (value: number) => {
    if (value >= 1000000) return `${(value / 1000000).toFixed(1)}M`;
    if (value >= 1000) return `${(value / 1000).toFixed(0)}K`;
    return value.toString();
  };

  return (
    <div className={`rounded-xl ${darkMode ? 'bg-slate-800 border-slate-700' : 'bg-white border-gray-200'} border p-4`}>
      <div className="flex items-center justify-between mb-4">
        <div>
          <h2 className={`text-lg font-semibold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
            {selectedPathogen || 'All Pathogens'} Outbreak Acceleration
          </h2>
          <p className={`text-sm ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
            Total Cases by Region (Live Data)
          </p>
        </div>
        <div className="flex gap-2">
          <button
            onClick={() => setChartMode('absolute')}
            className={`px-3 py-1.5 text-sm rounded-lg transition-colors ${
              chartMode === 'absolute'
                ? 'bg-blue-600 text-white'
                : darkMode ? 'bg-slate-700 text-slate-300' : 'bg-gray-100 text-gray-600'
            }`}
          >
            ABSOLUTE
          </button>
          <button
            onClick={() => setChartMode('growth')}
            className={`px-3 py-1.5 text-sm rounded-lg transition-colors ${
              chartMode === 'growth'
                ? 'bg-blue-600 text-white'
                : darkMode ? 'bg-slate-700 text-slate-300' : 'bg-gray-100 text-gray-600'
            }`}
          >
            GROWTH %
          </button>
        </div>
      </div>
      
      {/* Simple SVG Chart */}
      <div className="h-[300px] relative">
        <svg viewBox="0 0 600 280" className="w-full h-full">
          {/* Grid lines */}
          {[0, 1, 2, 3, 4].map(i => (
            <g key={i}>
              <line
                x1="50"
                y1={50 + i * 50}
                x2="580"
                y2={50 + i * 50}
                stroke={darkMode ? '#334155' : '#e5e7eb'}
                strokeDasharray="4"
              />
              <text
                x="45"
                y={55 + i * 50}
                textAnchor="end"
                fill={darkMode ? '#94a3b8' : '#6b7280'}
                fontSize="10"
              >
                {formatValue(maxValue - (maxValue / 4) * i)}
              </text>
            </g>
          ))}
          
          {/* X-axis labels */}
          {allWeeks.map((d, i) => (
            <text
              key={i}
              x={60 + i * (520 / (allWeeks.length - 1 || 1))}
              y="270"
              textAnchor="middle"
              fill={darkMode ? '#94a3b8' : '#6b7280'}
              fontSize="10"
            >
              W{d.week}
            </text>
          ))}
          
          {/* Data lines */}
          <defs>
            <linearGradient id="totalGrad" x1="0" y1="0" x2="0" y2="1">
              <stop offset="0%" stopColor="#f97316" stopOpacity="0.3" />
              <stop offset="100%" stopColor="#f97316" stopOpacity="0" />
            </linearGradient>
          </defs>
          
          {allWeeks.length > 1 && (
            <>
              <path
                d={`M${allWeeks.map((d, i) => {
                  const x = 60 + i * (520 / (allWeeks.length - 1));
                  const y = 250 - (d.total / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' L')} L${60 + (allWeeks.length - 1) * (520 / (allWeeks.length - 1))},250 L60,250 Z`}
                fill="url(#totalGrad)"
              />
              <polyline
                points={allWeeks.map((d, i) => {
                  const x = 60 + i * (520 / (allWeeks.length - 1));
                  const y = 250 - (d.total / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' ')}
                fill="none"
                stroke="#f97316"
                strokeWidth="3"
              />
              <polyline
                points={allWeeks.map((d, i) => {
                  const x = 60 + i * (520 / (allWeeks.length - 1));
                  const y = 250 - (d.usa / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' ')}
                fill="none"
                stroke="#3b82f6"
                strokeWidth="2"
              />
              <polyline
                points={allWeeks.map((d, i) => {
                  const x = 60 + i * (520 / (allWeeks.length - 1));
                  const y = 250 - (d.china / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' ')}
                fill="none"
                stroke="#dc2626"
                strokeWidth="2"
              />
              <polyline
                points={allWeeks.map((d, i) => {
                  const x = 60 + i * (520 / (allWeeks.length - 1));
                  const y = 250 - (d.eu / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' ')}
                fill="none"
                stroke="#eab308"
                strokeWidth="2"
              />
            </>
          )}
        </svg>
        
        {/* Legend */}
        <div className="absolute bottom-0 left-1/2 transform -translate-x-1/2 flex gap-4 text-xs">
          <div className="flex items-center gap-1">
            <span className="w-3 h-0.5 bg-orange-500"></span>
            <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>Global</span>
          </div>
          <div className="flex items-center gap-1">
            <span className="w-3 h-0.5 bg-blue-500"></span>
            <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>USA</span>
          </div>
          <div className="flex items-center gap-1">
            <span className="w-3 h-0.5 bg-red-600"></span>
            <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>China</span>
          </div>
          <div className="flex items-center gap-1">
            <span className="w-3 h-0.5 bg-yellow-500"></span>
            <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>EU</span>
          </div>
        </div>
      </div>
    </div>
  );
}

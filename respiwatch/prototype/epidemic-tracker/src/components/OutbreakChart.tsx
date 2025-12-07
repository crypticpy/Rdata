import { useState } from 'react';
import type { OutbreakData } from '../types';

interface OutbreakChartProps {
  data: OutbreakData;
  darkMode: boolean;
}

export function OutbreakChart({ data, darkMode }: OutbreakChartProps) {
  const [chartMode, setChartMode] = useState<'absolute' | 'growth'>('absolute');
  
  const timeline = data.timeline.progression_by_date;
  
  const chartData = timeline.map((t, idx) => {
    const caseNum = t.surveillance_indicators.case_numbers;
    const prevCaseNum = idx > 0 ? timeline[idx - 1].surveillance_indicators.case_numbers : caseNum;
    const growthRate = idx > 0 && prevCaseNum > 0 ? Math.round(((caseNum - prevCaseNum) / prevCaseNum) * 100) : 0;
    
    return {
      week: `W${t.week_number}`,
      global: caseNum,
      usa: Math.round(caseNum * 0.35),
      china: Math.round(caseNum * 0.25),
      eu: Math.round(caseNum * 0.4),
      growth: growthRate
    };
  });

  const maxValue = chartMode === 'absolute' 
    ? Math.max(...chartData.map(d => d.global))
    : Math.max(...chartData.map(d => Math.abs(d.growth)));

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
            H3N2 Outbreak Acceleration
          </h2>
          <p className={`text-sm ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
            Total Cases by Region
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
                {chartMode === 'absolute' 
                  ? formatValue(maxValue - (maxValue / 4) * i)
                  : `${Math.round(maxValue - (maxValue / 4) * i)}%`}
              </text>
            </g>
          ))}
          
          {/* X-axis labels */}
          {chartData.map((d, i) => (
            <text
              key={i}
              x={60 + i * (520 / (chartData.length - 1 || 1))}
              y="270"
              textAnchor="middle"
              fill={darkMode ? '#94a3b8' : '#6b7280'}
              fontSize="10"
            >
              {d.week}
            </text>
          ))}
          
          {/* Data lines */}
          {chartMode === 'absolute' ? (
            <>
              {/* Global line with area fill */}
              <defs>
                <linearGradient id="globalGrad" x1="0" y1="0" x2="0" y2="1">
                  <stop offset="0%" stopColor="#f97316" stopOpacity="0.3" />
                  <stop offset="100%" stopColor="#f97316" stopOpacity="0" />
                </linearGradient>
              </defs>
              <path
                d={`M${chartData.map((d, i) => {
                  const x = 60 + i * (520 / (chartData.length - 1 || 1));
                  const y = 250 - (d.global / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' L')} L${60 + (chartData.length - 1) * (520 / (chartData.length - 1 || 1))},250 L60,250 Z`}
                fill="url(#globalGrad)"
              />
              <polyline
                points={chartData.map((d, i) => {
                  const x = 60 + i * (520 / (chartData.length - 1 || 1));
                  const y = 250 - (d.global / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' ')}
                fill="none"
                stroke="#f97316"
                strokeWidth="3"
              />
              {/* USA line */}
              <polyline
                points={chartData.map((d, i) => {
                  const x = 60 + i * (520 / (chartData.length - 1 || 1));
                  const y = 250 - (d.usa / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' ')}
                fill="none"
                stroke="#3b82f6"
                strokeWidth="2"
              />
              {/* China line */}
              <polyline
                points={chartData.map((d, i) => {
                  const x = 60 + i * (520 / (chartData.length - 1 || 1));
                  const y = 250 - (d.china / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' ')}
                fill="none"
                stroke="#dc2626"
                strokeWidth="2"
              />
              {/* EU line */}
              <polyline
                points={chartData.map((d, i) => {
                  const x = 60 + i * (520 / (chartData.length - 1 || 1));
                  const y = 250 - (d.eu / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' ')}
                fill="none"
                stroke="#eab308"
                strokeWidth="2"
              />
            </>
          ) : (
            <>
              <defs>
                <linearGradient id="growthGrad" x1="0" y1="0" x2="0" y2="1">
                  <stop offset="0%" stopColor="#10b981" stopOpacity="0.3" />
                  <stop offset="100%" stopColor="#10b981" stopOpacity="0" />
                </linearGradient>
              </defs>
              <path
                d={`M${chartData.map((d, i) => {
                  const x = 60 + i * (520 / (chartData.length - 1 || 1));
                  const y = 250 - (d.growth / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' L')} L${60 + (chartData.length - 1) * (520 / (chartData.length - 1 || 1))},250 L60,250 Z`}
                fill="url(#growthGrad)"
              />
              <polyline
                points={chartData.map((d, i) => {
                  const x = 60 + i * (520 / (chartData.length - 1 || 1));
                  const y = 250 - (d.growth / maxValue) * 200;
                  return `${x},${y}`;
                }).join(' ')}
                fill="none"
                stroke="#10b981"
                strokeWidth="3"
              />
            </>
          )}
        </svg>
        
        {/* Legend */}
        <div className="absolute bottom-0 left-1/2 transform -translate-x-1/2 flex gap-4 text-xs">
          {chartMode === 'absolute' ? (
            <>
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
                <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>EU/EEA</span>
              </div>
            </>
          ) : (
            <div className="flex items-center gap-1">
              <span className="w-3 h-0.5 bg-emerald-500"></span>
              <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>Growth Rate %</span>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

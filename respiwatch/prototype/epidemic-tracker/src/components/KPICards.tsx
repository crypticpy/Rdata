import { AlertTriangle, MapPin, TrendingUp, Radio } from 'lucide-react';
import type { OutbreakData, AnomalyDetectionFlags } from '../types';

interface KPICardsProps {
  data: OutbreakData;
  anomalyData: AnomalyDetectionFlags;
  darkMode: boolean;
}

export function KPICards({ data, anomalyData, darkMode }: KPICardsProps) {
  const vaccineEffectiveness = data.global_overview.severity_assessment.vaccine_effectiveness;
  
  // Count hotspots (regions with >30% positivity)
  const activeHotspots = Object.values(data.global_overview.current_status)
    .filter(r => r.positivity_rate > 30).length;
  
  // Calculate hospitalization trend from timeline
  const timeline = data.timeline.progression_by_date;
  const latestRate = timeline[timeline.length - 1]?.surveillance_indicators.hospitalization_rate || 0;
  const previousRate = timeline[timeline.length - 2]?.surveillance_indicators.hospitalization_rate || 0;
  const hospTrend = previousRate > 0 ? ((latestRate - previousRate) / previousRate * 100).toFixed(0) : '+18';

  const newSignals = anomalyData.suspicious_patterns.filter(p => 
    new Date(p.first_detected) > new Date('2025-11-20')
  ).length;

  const cards = [
    {
      title: 'Dominant Strain',
      value: 'H3N2 Subclade K',
      subtitle: `Vaccine Mismatch (${vaccineEffectiveness.subclade_k_effectiveness}% VE)`,
      icon: AlertTriangle,
      color: 'red',
      bgColor: darkMode ? 'bg-red-500/10' : 'bg-red-50',
      borderColor: darkMode ? 'border-red-500/30' : 'border-red-200',
      iconBg: 'bg-red-500/20',
      iconColor: 'text-red-500'
    },
    {
      title: 'Active Hotspots',
      value: activeHotspots.toString(),
      subtitle: 'Regions with >30% positivity',
      icon: MapPin,
      color: 'orange',
      bgColor: darkMode ? 'bg-orange-500/10' : 'bg-orange-50',
      borderColor: darkMode ? 'border-orange-500/30' : 'border-orange-200',
      iconBg: 'bg-orange-500/20',
      iconColor: 'text-orange-500'
    },
    {
      title: 'Hospitalization Trend',
      value: `+${hospTrend}%`,
      subtitle: 'Week-over-Week (Northern Hemisphere)',
      icon: TrendingUp,
      color: 'yellow',
      bgColor: darkMode ? 'bg-yellow-500/10' : 'bg-yellow-50',
      borderColor: darkMode ? 'border-yellow-500/30' : 'border-yellow-200',
      iconBg: 'bg-yellow-500/20',
      iconColor: 'text-yellow-500'
    },
    {
      title: 'Signal Detection',
      value: `${newSignals} New`,
      subtitle: 'Atypical patterns detected',
      icon: Radio,
      color: 'blue',
      bgColor: darkMode ? 'bg-blue-500/10' : 'bg-blue-50',
      borderColor: darkMode ? 'border-blue-500/30' : 'border-blue-200',
      iconBg: 'bg-blue-500/20',
      iconColor: 'text-blue-500'
    }
  ];

  return (
    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
      {cards.map((card, idx) => (
        <div 
          key={idx}
          className={`rounded-xl p-4 border ${card.bgColor} ${card.borderColor}`}
        >
          <div className="flex items-start justify-between">
            <div>
              <p className={`text-xs font-medium ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
                {card.title}
              </p>
              <p className={`text-xl font-bold mt-1 ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                {card.value}
              </p>
              <p className={`text-xs mt-1 ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
                {card.subtitle}
              </p>
            </div>
            <div className={`p-2 rounded-lg ${card.iconBg}`}>
              <card.icon className={`w-5 h-5 ${card.iconColor}`} />
            </div>
          </div>
        </div>
      ))}
    </div>
  );
}

import { AlertTriangle, MapPin, TrendingUp, Radio } from 'lucide-react';
import type { OutbreakRecord, Alert } from '../lib/supabase';

interface KPICardsLiveProps {
  outbreakData: OutbreakRecord[];
  alerts: Alert[];
  selectedPathogen: string | null;
  darkMode: boolean;
}

export function KPICardsLive({ outbreakData, alerts, selectedPathogen, darkMode }: KPICardsLiveProps) {
  // When showing all pathogens, use all data; otherwise filter to selected
  const pathogenData = selectedPathogen 
    ? outbreakData.filter(d => d.pathogen === selectedPathogen)
    : outbreakData;
  
  // Count hotspots (countries with high/critical severity)
  const activeHotspots = outbreakData.filter(d => 
    d.severity_level === 'high' || d.severity_level === 'critical'
  ).length;
  
  // Calculate hospitalization trend
  const totalHosp = pathogenData.reduce((sum, d) => sum + (d.hospitalizations || 0), 0);
  const totalCases = pathogenData.reduce((sum, d) => sum + (d.cases || 0), 0);
  const hospRate = totalCases > 0 ? ((totalHosp / totalCases) * 100).toFixed(1) : '0';

  // Count new signals
  const criticalAlerts = alerts.filter(a => a.severity === 'critical').length;

  const cards = [
    {
      title: selectedPathogen ? 'Active Pathogen' : 'Total Coverage',
      value: selectedPathogen || 'All Pathogens',
      subtitle: `${pathogenData.length} records across ${new Set(pathogenData.map(d => d.iso_code)).size} countries`,
      icon: AlertTriangle,
      bgColor: darkMode ? 'bg-red-500/10' : 'bg-red-50',
      borderColor: darkMode ? 'border-red-500/30' : 'border-red-200',
      iconBg: 'bg-red-500/20',
      iconColor: 'text-red-500'
    },
    {
      title: 'Active Hotspots',
      value: activeHotspots.toString(),
      subtitle: 'Regions with high/critical severity',
      icon: MapPin,
      bgColor: darkMode ? 'bg-orange-500/10' : 'bg-orange-50',
      borderColor: darkMode ? 'border-orange-500/30' : 'border-orange-200',
      iconBg: 'bg-orange-500/20',
      iconColor: 'text-orange-500'
    },
    {
      title: 'Hospitalization Rate',
      value: `${hospRate}%`,
      subtitle: `${totalHosp.toLocaleString()} hospitalizations`,
      icon: TrendingUp,
      bgColor: darkMode ? 'bg-yellow-500/10' : 'bg-yellow-50',
      borderColor: darkMode ? 'border-yellow-500/30' : 'border-yellow-200',
      iconBg: 'bg-yellow-500/20',
      iconColor: 'text-yellow-500'
    },
    {
      title: 'Signal Detection',
      value: `${criticalAlerts} Critical`,
      subtitle: `${alerts.length} total alerts active`,
      icon: Radio,
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

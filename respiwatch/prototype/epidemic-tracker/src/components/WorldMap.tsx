import { ComposableMap, Geographies, Geography, ZoomableGroup } from 'react-simple-maps';
import type { OutbreakData } from '../types';

const geoUrl = 'https://cdn.jsdelivr.net/npm/world-atlas@2/countries-110m.json';

interface WorldMapProps {
  data: OutbreakData;
  onCountryClick: (iso: string | null) => void;
  selectedCountry: string | null;
  darkMode: boolean;
}

const countryIsoMapping: Record<string, string> = {
  'United States of America': 'USA',
  'United States': 'USA',
  'United Kingdom': 'GBR',
  'Japan': 'JPN',
  'Australia': 'AUS',
  'Canada': 'CAN',
  'China': 'CHN',
  'France': 'FRA',
  'Germany': 'DEU',
  'Italy': 'ITA',
  'Spain': 'ESP'
};

function getCountryColor(iso: string, data: OutbreakData): string {
  const countryData = data.countries[iso];
  if (!countryData) return '#334155'; // default gray
  
  const positivityRate = countryData.h3n2_data.positivity_rate;
  const status = countryData.outbreak_status;
  
  if (status === 'peak' || positivityRate > 35) return '#dc2626'; // red - critical
  if (status === 'active_outbreak' || positivityRate > 25) return '#f97316'; // orange - high
  if (status === 'elevated_activity' || positivityRate > 15) return '#eab308'; // yellow - moderate
  return '#3b82f6'; // blue - low
}

export function WorldMap({ data, onCountryClick, selectedCountry, darkMode }: WorldMapProps) {
  return (
    <div className="w-full h-[400px] relative">
      <ComposableMap
        projectionConfig={{
          rotate: [-10, 0, 0],
          scale: 147
        }}
        style={{ width: '100%', height: '100%' }}
      >
        <ZoomableGroup center={[0, 20]} zoom={1}>
          <Geographies geography={geoUrl}>
            {({ geographies }) =>
              geographies.map((geo) => {
                const countryName = geo.properties.name;
                const iso = countryIsoMapping[countryName] || geo.properties.ISO_A3 || geo.id;
                const fillColor = getCountryColor(iso, data);
                const isSelected = selectedCountry === iso;
                
                return (
                  <Geography
                    key={geo.rsmKey}
                    geography={geo}
                    onClick={() => {
                      if (data.countries[iso]) {
                        onCountryClick(iso);
                      }
                    }}
                    style={{
                      default: {
                        fill: fillColor,
                        stroke: darkMode ? '#1e293b' : '#e5e7eb',
                        strokeWidth: 0.5,
                        outline: 'none',
                        cursor: data.countries[iso] ? 'pointer' : 'default',
                        opacity: isSelected ? 1 : 0.85
                      },
                      hover: {
                        fill: fillColor,
                        stroke: '#fff',
                        strokeWidth: 1,
                        outline: 'none',
                        opacity: 1
                      },
                      pressed: {
                        fill: fillColor,
                        stroke: '#fff',
                        strokeWidth: 1.5,
                        outline: 'none'
                      }
                    }}
                  />
                );
              })
            }
          </Geographies>
        </ZoomableGroup>
      </ComposableMap>
    </div>
  );
}

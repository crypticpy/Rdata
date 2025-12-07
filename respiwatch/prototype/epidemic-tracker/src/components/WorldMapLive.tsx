import { ComposableMap, Geographies, Geography, ZoomableGroup } from 'react-simple-maps';
import type { OutbreakRecord } from '../lib/supabase';

const geoUrl = 'https://cdn.jsdelivr.net/npm/world-atlas@2/countries-110m.json';

interface WorldMapLiveProps {
  data: OutbreakRecord[];
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
  'Spain': 'ESP',
  'Russia': 'RUS',
  'Russian Federation': 'RUS',
  'India': 'IND',
  'Brazil': 'BRA',
  'South Africa': 'ZAF',
  'Nigeria': 'NGA',
  'Egypt': 'EGY',
  'Argentina': 'ARG',
  'Colombia': 'COL',
  'Chile': 'CHL',
  'Kenya': 'KEN',
  'Mexico': 'MEX',
  'Indonesia': 'IDN',
  'Pakistan': 'PAK',
  'Bangladesh': 'BGD',
  'Vietnam': 'VNM',
  'Philippines': 'PHL',
  'Thailand': 'THA',
  'Malaysia': 'MYS',
  'Singapore': 'SGP',
  'South Korea': 'KOR',
  'Republic of Korea': 'KOR',
  'Korea': 'KOR',
  'Saudi Arabia': 'SAU',
  'United Arab Emirates': 'ARE',
  'Iran': 'IRN',
  'Turkey': 'TUR',
  'Poland': 'POL',
  'Netherlands': 'NLD',
  'Belgium': 'BEL',
  'Sweden': 'SWE',
  'Norway': 'NOR',
  'Denmark': 'DNK',
  'Finland': 'FIN',
  'Switzerland': 'CHE',
  'Austria': 'AUT',
  'Portugal': 'PRT',
  'Greece': 'GRC',
  'Czechia': 'CZE',
  'Czech Republic': 'CZE',
  'Romania': 'ROU',
  'Hungary': 'HUN',
  'Ukraine': 'UKR',
  'Peru': 'PER',
  'Venezuela': 'VEN',
  'Ecuador': 'ECU',
  'Bolivia': 'BOL',
  'Paraguay': 'PRY',
  'Uruguay': 'URY',
  'Morocco': 'MAR',
  'Algeria': 'DZA',
  'Tunisia': 'TUN',
  'Libya': 'LBY',
  'Sudan': 'SDN',
  'Ethiopia': 'ETH',
  'Tanzania': 'TZA',
  'Democratic Republic of the Congo': 'COD',
  'Dem. Rep. Congo': 'COD',
  'Congo': 'COG',
  'Ghana': 'GHA',
  'Cameroon': 'CMR',
  'Uganda': 'UGA',
  'Angola': 'AGO',
  'Mozambique': 'MOZ',
  'Zimbabwe': 'ZWE',
  'Zambia': 'ZMB',
  'New Zealand': 'NZL',
  'Papua New Guinea': 'PNG',
  'Fiji': 'FJI',
  'Israel': 'ISR',
  'Jordan': 'JOR',
  'Iraq': 'IRQ',
  'Syria': 'SYR',
  'Lebanon': 'LBN',
  'Kuwait': 'KWT',
  'Qatar': 'QAT',
  'Bahrain': 'BHR',
  'Oman': 'OMN',
  'Yemen': 'YEM',
  'Afghanistan': 'AFG',
  'Myanmar': 'MMR',
  'Burma': 'MMR',
  'Cambodia': 'KHM',
  'Laos': 'LAO',
  'Nepal': 'NPL',
  'Sri Lanka': 'LKA',
  'Kazakhstan': 'KAZ',
  'Uzbekistan': 'UZB',
  'Turkmenistan': 'TKM',
  'Mongolia': 'MNG',
  'North Korea': 'PRK',
  'Taiwan': 'TWN',
  'Hong Kong': 'HKG',
  'Macau': 'MAC',
  'Ireland': 'IRL',
  'Iceland': 'ISL',
  'Luxembourg': 'LUX',
  'Slovenia': 'SVN',
  'Slovakia': 'SVK',
  'Croatia': 'HRV',
  'Serbia': 'SRB',
  'Bosnia and Herz.': 'BIH',
  'Bosnia and Herzegovina': 'BIH',
  'Montenegro': 'MNE',
  'North Macedonia': 'MKD',
  'Macedonia': 'MKD',
  'Albania': 'ALB',
  'Kosovo': 'XKX',
  'Moldova': 'MDA',
  'Belarus': 'BLR',
  'Lithuania': 'LTU',
  'Latvia': 'LVA',
  'Estonia': 'EST',
  'Georgia': 'GEO',
  'Armenia': 'ARM',
  'Azerbaijan': 'AZE',
  'Cuba': 'CUB',
  'Dominican Rep.': 'DOM',
  'Dominican Republic': 'DOM',
  'Haiti': 'HTI',
  'Jamaica': 'JAM',
  'Puerto Rico': 'PRI',
  'Trinidad and Tobago': 'TTO',
  'Guatemala': 'GTM',
  'Honduras': 'HND',
  'El Salvador': 'SLV',
  'Nicaragua': 'NIC',
  'Costa Rica': 'CRI',
  'Panama': 'PAN'
};

function getCountryColor(iso: string, data: OutbreakRecord[]): string {
  const countryData = data.find(d => d.iso_code === iso);
  if (!countryData) return '#334155'; // default gray
  
  const severity = countryData.severity_level;
  
  if (severity === 'critical') return '#dc2626'; // red
  if (severity === 'high') return '#f97316'; // orange
  if (severity === 'moderate') return '#eab308'; // yellow
  return '#3b82f6'; // blue - low
}

export function WorldMapLive({ data, onCountryClick, selectedCountry, darkMode }: WorldMapLiveProps) {
  const countryIsos = new Set(data.map(d => d.iso_code));
  
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
                const hasData = countryIsos.has(iso);
                
                return (
                  <Geography
                    key={geo.rsmKey}
                    geography={geo}
                    onClick={() => {
                      if (hasData) {
                        onCountryClick(iso);
                      }
                    }}
                    style={{
                      default: {
                        fill: fillColor,
                        stroke: darkMode ? '#1e293b' : '#e5e7eb',
                        strokeWidth: 0.5,
                        outline: 'none',
                        cursor: hasData ? 'pointer' : 'default',
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

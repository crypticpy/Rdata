import { useState, useEffect, useCallback } from 'react';
import { WorldMapLive } from './components/WorldMapLive';
import { KPICardsLive } from './components/KPICardsLive';
import { TimelineControl } from './components/TimelineControl';
import { OutbreakChartLive } from './components/OutbreakChartLive';
import { SuspiciousSignalsLive } from './components/SuspiciousSignalsLive';
import { PriorityZonesLive } from './components/PriorityZonesLive';
import { AnalystNotesLive } from './components/AnalystNotesLive';
import { CountryDetailLive } from './components/CountryDetailLive';
import { PathogenSelector } from './components/PathogenSelector';
import { RefreshCw, Activity, AlertCircle, Database } from 'lucide-react';
import { 
  fetchOutbreakData, 
  fetchAlerts, 
  fetchPathogens, 
  refreshLiveData,
  getOutbreakByCountry,
  getDataSourceStatus,
  type OutbreakRecord, 
  type Alert, 
  type Pathogen,
  type SourceStatus
} from './lib/supabase';
import { DataSourceStatus } from './components/DataSourceStatus';

function App() {
  const [outbreakData, setOutbreakData] = useState<OutbreakRecord[]>([]);
  const [countryData, setCountryData] = useState<OutbreakRecord[]>([]);
  const [alerts, setAlerts] = useState<Alert[]>([]);
  const [pathogens, setPathogens] = useState<Pathogen[]>([]);
  const [selectedPathogen, setSelectedPathogen] = useState<string | null>(null);
  const [selectedWeek, setSelectedWeek] = useState(48);
  const [selectedCountry, setSelectedCountry] = useState<string | null>(null);
  const [isPlaying, setIsPlaying] = useState(false);
  const [isLoading, setIsLoading] = useState(true);
  const [isRefreshing, setIsRefreshing] = useState(false);
  const [lastRefresh, setLastRefresh] = useState<Date | null>(null);
  const [darkMode, setDarkMode] = useState(true);
  const [dataSource, setDataSource] = useState<'live' | 'cached'>('cached');
  const [error, setError] = useState<string | null>(null);
  const [sourceStatus, setSourceStatus] = useState<Record<string, SourceStatus>>({});

  const loadData = useCallback(async () => {
    setIsLoading(true);
    setError(null);
    try {
      const [outbreakRes, alertsRes, pathogensRes, statusRes] = await Promise.all([
        fetchOutbreakData(selectedPathogen || undefined),
        fetchAlerts(),
        fetchPathogens(),
        getDataSourceStatus()
      ]);
      
      const countryRes = await getOutbreakByCountry(selectedPathogen || undefined);
      
      setOutbreakData(outbreakRes);
      setCountryData(countryRes);
      setAlerts(alertsRes);
      setPathogens(pathogensRes);
      setSourceStatus(statusRes);
      setLastRefresh(new Date());
      setDataSource('cached');
    } catch (err) {
      console.error('Error loading data:', err);
      setError('Failed to load surveillance data from database');
    } finally {
      setIsLoading(false);
    }
  }, [selectedPathogen]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  useEffect(() => {
    if (isPlaying) {
      const interval = setInterval(() => {
        setSelectedWeek(w => w < 49 ? w + 1 : 40);
      }, 1000);
      return () => clearInterval(interval);
    }
  }, [isPlaying]);

  const handleRefreshLive = async () => {
    setIsRefreshing(true);
    try {
      const result = await refreshLiveData();
      if (result?.sources) {
        setSourceStatus(result.sources);
      }
      await loadData();
      setDataSource('live');
    } catch (err) {
      console.error('Error refreshing live data:', err);
      setError('Failed to refresh live data from external sources');
    } finally {
      setIsRefreshing(false);
    }
  };

  if (isLoading) {
    return (
      <div className={`min-h-screen flex items-center justify-center ${darkMode ? 'bg-slate-900' : 'bg-gray-50'}`}>
        <div className="flex flex-col items-center gap-4">
          <RefreshCw className={`w-12 h-12 animate-spin ${darkMode ? 'text-blue-400' : 'text-blue-600'}`} />
          <p className={darkMode ? 'text-slate-300' : 'text-gray-600'}>Loading surveillance data from Supabase...</p>
        </div>
      </div>
    );
  }

  if (error && outbreakData.length === 0) {
    return (
      <div className={`min-h-screen flex items-center justify-center ${darkMode ? 'bg-slate-900' : 'bg-gray-50'}`}>
        <div className="flex flex-col items-center gap-4 text-red-500">
          <AlertCircle className="w-12 h-12" />
          <p>{error}</p>
          <button onClick={loadData} className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700">
            Retry
          </button>
        </div>
      </div>
    );
  }

  const currentDate = new Date().toLocaleDateString('en-US', { 
    month: 'long', 
    day: 'numeric', 
    year: 'numeric' 
  });

  return (
    <div className={`min-h-screen ${darkMode ? 'bg-slate-900 text-slate-100' : 'bg-gray-50 text-gray-900'}`}>
      {/* Header */}
      <header className={`sticky top-0 z-50 ${darkMode ? 'bg-slate-800/95 border-slate-700' : 'bg-white/95 border-gray-200'} border-b backdrop-blur-sm`}>
        <div className="max-w-[1800px] mx-auto px-4 py-3 flex items-center justify-between">
          <div className="flex items-center gap-4">
            <h1 className={`text-xl font-bold tracking-wide ${darkMode ? 'text-white' : 'text-gray-900'}`}>
              GLOBAL RESPIRATORY SURVEILLANCE
            </h1>
            <span className={`text-sm ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
              Multi-Pathogen Tracking - 2025 Season
            </span>
          </div>
          <div className="flex items-center gap-4">
            <div className={`flex items-center gap-2 px-3 py-1.5 rounded-full text-sm ${
              dataSource === 'live' 
                ? 'bg-green-500/20 text-green-400' 
                : 'bg-blue-500/20 text-blue-400'
            }`}>
              {dataSource === 'live' ? <Activity className="w-4 h-4" /> : <Database className="w-4 h-4" />}
              <span>{dataSource === 'live' ? 'LIVE DATA' : 'CACHED DATA'}</span>
            </div>
            <button
              onClick={handleRefreshLive}
              disabled={isRefreshing}
              className={`flex items-center gap-2 px-3 py-1.5 rounded-lg ${
                darkMode ? 'bg-slate-700 hover:bg-slate-600 text-slate-200' : 'bg-gray-100 hover:bg-gray-200 text-gray-700'
              } transition-colors disabled:opacity-50`}
            >
              <RefreshCw className={`w-4 h-4 ${isRefreshing ? 'animate-spin' : ''}`} />
              <span className="text-sm">{isRefreshing ? 'Fetching...' : 'Refresh Live'}</span>
            </button>
            <div className={`text-sm ${darkMode ? 'text-slate-400' : 'text-gray-500'}`}>
              {currentDate}
            </div>
            <button
              onClick={() => setDarkMode(!darkMode)}
              className={`px-3 py-1.5 rounded-lg text-sm ${
                darkMode ? 'bg-slate-700 text-slate-200' : 'bg-gray-200 text-gray-700'
              }`}
            >
              {darkMode ? 'Light' : 'Dark'}
            </button>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="max-w-[1800px] mx-auto px-4 py-6">
        {/* Pathogen Selector */}
        <PathogenSelector 
          pathogens={pathogens}
          selectedPathogen={selectedPathogen}
          onSelect={setSelectedPathogen}
          darkMode={darkMode}
        />

        {/* KPI Cards */}
        <KPICardsLive 
          outbreakData={countryData} 
          alerts={alerts} 
          selectedPathogen={selectedPathogen}
          darkMode={darkMode} 
        />

        {/* Data Source Status */}
        {Object.keys(sourceStatus).length > 0 && (
          <div className="mt-4">
            <DataSourceStatus sources={sourceStatus} darkMode={darkMode} />
          </div>
        )}

        {/* Main Grid */}
        <div className="grid grid-cols-12 gap-6 mt-6">
          {/* Left Column - Map and Chart */}
          <div className="col-span-12 lg:col-span-8 space-y-6">
            {/* World Map */}
            <div className={`rounded-xl ${darkMode ? 'bg-slate-800 border-slate-700' : 'bg-white border-gray-200'} border p-4`}>
              <div className="flex items-center justify-between mb-4">
                <h2 className={`text-lg font-semibold ${darkMode ? 'text-white' : 'text-gray-900'}`}>
                  Global Outbreak Distribution {selectedPathogen ? `- ${selectedPathogen}` : '- All Pathogens'}
                </h2>
                <div className="flex items-center gap-4">
                  <div className="flex items-center gap-2 text-xs">
                    <span className="w-3 h-3 rounded-full bg-blue-400"></span>
                    <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>Low</span>
                  </div>
                  <div className="flex items-center gap-2 text-xs">
                    <span className="w-3 h-3 rounded-full bg-yellow-400"></span>
                    <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>Moderate</span>
                  </div>
                  <div className="flex items-center gap-2 text-xs">
                    <span className="w-3 h-3 rounded-full bg-orange-500"></span>
                    <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>High</span>
                  </div>
                  <div className="flex items-center gap-2 text-xs">
                    <span className="w-3 h-3 rounded-full bg-red-600"></span>
                    <span className={darkMode ? 'text-slate-400' : 'text-gray-500'}>Critical</span>
                  </div>
                </div>
              </div>
              <WorldMapLive 
                data={countryData} 
                onCountryClick={setSelectedCountry}
                selectedCountry={selectedCountry}
                darkMode={darkMode}
              />
              <TimelineControl 
                selectedWeek={selectedWeek}
                onWeekChange={setSelectedWeek}
                isPlaying={isPlaying}
                onPlayPause={() => setIsPlaying(!isPlaying)}
                darkMode={darkMode}
              />
            </div>

            {/* Outbreak Chart */}
            <OutbreakChartLive 
              data={outbreakData} 
              selectedPathogen={selectedPathogen}
              darkMode={darkMode} 
            />
          </div>

          {/* Right Column - Signals and Priority Zones */}
          <div className="col-span-12 lg:col-span-4 space-y-6">
            <SuspiciousSignalsLive alerts={alerts} darkMode={darkMode} />
            <PriorityZonesLive 
              data={countryData} 
              onCountryClick={setSelectedCountry} 
              darkMode={darkMode} 
            />
            <AnalystNotesLive 
              alerts={alerts}
              pathogens={pathogens}
              selectedPathogen={selectedPathogen}
              darkMode={darkMode} 
            />
          </div>
        </div>

        {/* Last Refresh Info */}
        {lastRefresh && (
          <div className={`mt-6 text-center text-xs ${darkMode ? 'text-slate-500' : 'text-gray-400'}`}>
            Last updated: {lastRefresh.toLocaleString()} | Data sources: WHO, CDC, ECDC, UKHSA, OWID
          </div>
        )}
      </main>

      {/* Country Detail Modal */}
      {selectedCountry && (
        <CountryDetailLive 
          countryCode={selectedCountry}
          data={countryData}
          onClose={() => setSelectedCountry(null)}
          darkMode={darkMode}
        />
      )}
    </div>
  );
}

export default App;

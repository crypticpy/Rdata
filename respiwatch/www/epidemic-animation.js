/**
 * RespiWatch Epidemic Animation Controller
 * Handles time-lapse visualization of outbreak spread with play/pause/speed controls
 */

(function() {
  'use strict';

  // Animation state
  const AnimationState = {
    isPlaying: false,
    currentFrame: 0,
    totalFrames: 0,
    speed: 1, // 1 = normal, 0.5 = slow, 2 = fast
    intervalId: null,
    frameData: [],
    currentLayer: 'cases', // cases, rt, capacity
    mapId: 'global_map'
  };

  // Speed presets (milliseconds per frame)
  const SPEED_PRESETS = {
    slow: 2000,
    normal: 1000,
    fast: 500,
    veryFast: 250
  };

  /**
   * Initialize the animation controller
   * @param {Object} config - Configuration options
   */
  function initializeAnimation(config = {}) {
    AnimationState.mapId = config.mapId || 'global_map';
    AnimationState.totalFrames = config.totalFrames || 0;
    AnimationState.frameData = config.frameData || [];
    AnimationState.currentLayer = config.layer || 'cases';

    // Set up event listeners
    setupEventListeners();

    // Update UI
    updateControlsUI();

    console.log('Epidemic animation initialized', AnimationState);
  }

  /**
   * Set up event listeners for animation controls
   */
  function setupEventListeners() {
    // Play/Pause button
    $(document).on('click', '#animation_play_pause', function() {
      if (AnimationState.isPlaying) {
        pauseAnimation();
      } else {
        playAnimation();
      }
    });

    // Stop button
    $(document).on('click', '#animation_stop', function() {
      stopAnimation();
    });

    // Speed selector
    $(document).on('change', '#animation_speed', function() {
      const speed = $(this).val();
      setAnimationSpeed(speed);
    });

    // Timeline slider
    $(document).on('input', '#animation_timeline', function() {
      const frame = parseInt($(this).val());
      seekToFrame(frame);
    });

    // Layer toggle buttons
    $(document).on('click', '.layer-toggle-btn', function() {
      const layer = $(this).data('layer');
      setMapLayer(layer);
    });

    // Step forward/backward
    $(document).on('click', '#animation_step_forward', function() {
      stepForward();
    });

    $(document).on('click', '#animation_step_backward', function() {
      stepBackward();
    });
  }

  /**
   * Play the animation
   */
  function playAnimation() {
    if (AnimationState.isPlaying) return;

    AnimationState.isPlaying = true;
    updatePlayButtonUI(true);

    const interval = getIntervalForSpeed(AnimationState.speed);

    AnimationState.intervalId = setInterval(function() {
      if (AnimationState.currentFrame >= AnimationState.totalFrames - 1) {
        // Loop back to start or stop
        AnimationState.currentFrame = 0;
      } else {
        AnimationState.currentFrame++;
      }

      updateMapFrame(AnimationState.currentFrame);
      updateTimelineUI();
    }, interval);

    // Notify Shiny
    Shiny.setInputValue('animation_playing', true, {priority: 'event'});
  }

  /**
   * Pause the animation
   */
  function pauseAnimation() {
    AnimationState.isPlaying = false;
    updatePlayButtonUI(false);

    if (AnimationState.intervalId) {
      clearInterval(AnimationState.intervalId);
      AnimationState.intervalId = null;
    }

    Shiny.setInputValue('animation_playing', false, {priority: 'event'});
  }

  /**
   * Stop and reset the animation
   */
  function stopAnimation() {
    pauseAnimation();
    AnimationState.currentFrame = 0;
    updateMapFrame(0);
    updateTimelineUI();
  }

  /**
   * Set animation speed
   * @param {string} speed - Speed preset name
   */
  function setAnimationSpeed(speed) {
    AnimationState.speed = speed;

    // If playing, restart with new speed
    if (AnimationState.isPlaying) {
      pauseAnimation();
      playAnimation();
    }

    Shiny.setInputValue('animation_speed', speed, {priority: 'event'});
  }

  /**
   * Get interval in ms for given speed
   * @param {string} speed - Speed preset
   * @returns {number} Interval in milliseconds
   */
  function getIntervalForSpeed(speed) {
    return SPEED_PRESETS[speed] || SPEED_PRESETS.normal;
  }

  /**
   * Seek to a specific frame
   * @param {number} frame - Frame number
   */
  function seekToFrame(frame) {
    if (frame < 0 || frame >= AnimationState.totalFrames) return;

    AnimationState.currentFrame = frame;
    updateMapFrame(frame);
    updateTimelineUI();

    Shiny.setInputValue('animation_frame', frame, {priority: 'event'});
  }

  /**
   * Step forward one frame
   */
  function stepForward() {
    if (AnimationState.currentFrame < AnimationState.totalFrames - 1) {
      seekToFrame(AnimationState.currentFrame + 1);
    }
  }

  /**
   * Step backward one frame
   */
  function stepBackward() {
    if (AnimationState.currentFrame > 0) {
      seekToFrame(AnimationState.currentFrame - 1);
    }
  }

  /**
   * Set the map display layer
   * @param {string} layer - Layer name (cases, rt, capacity)
   */
  function setMapLayer(layer) {
    AnimationState.currentLayer = layer;

    // Update button states
    $('.layer-toggle-btn').removeClass('active');
    $(`.layer-toggle-btn[data-layer="${layer}"]`).addClass('active');

    // Notify Shiny to update map colors
    Shiny.setInputValue('animation_layer', layer, {priority: 'event'});

    // Re-render current frame with new layer
    updateMapFrame(AnimationState.currentFrame);
  }

  /**
   * Update the map for a specific frame
   * @param {number} frame - Frame number
   */
  function updateMapFrame(frame) {
    if (!AnimationState.frameData || frame >= AnimationState.frameData.length) {
      // Request frame data from Shiny
      Shiny.setInputValue('animation_request_frame', {
        frame: frame,
        layer: AnimationState.currentLayer
      }, {priority: 'event'});
      return;
    }

    const frameData = AnimationState.frameData[frame];

    // Update map via Leaflet proxy
    if (typeof Shiny !== 'undefined') {
      Shiny.setInputValue('animation_update_map', {
        frame: frame,
        data: frameData,
        layer: AnimationState.currentLayer
      }, {priority: 'event'});
    }

    // Update date display
    if (frameData && frameData.date) {
      $('#animation_current_date').text(frameData.date);
    }
  }

  /**
   * Load frame data from Shiny
   * @param {Array} data - Array of frame data objects
   */
  function loadFrameData(data) {
    AnimationState.frameData = data;
    AnimationState.totalFrames = data.length;

    // Update timeline slider max
    $('#animation_timeline').attr('max', AnimationState.totalFrames - 1);

    updateTimelineUI();
  }

  /**
   * Update the play/pause button UI
   * @param {boolean} isPlaying - Current play state
   */
  function updatePlayButtonUI(isPlaying) {
    const btn = $('#animation_play_pause');
    const icon = btn.find('i');

    if (isPlaying) {
      icon.removeClass('fa-play').addClass('fa-pause');
      btn.attr('title', 'Pause');
    } else {
      icon.removeClass('fa-pause').addClass('fa-play');
      btn.attr('title', 'Play');
    }
  }

  /**
   * Update timeline slider and frame counter UI
   */
  function updateTimelineUI() {
    $('#animation_timeline').val(AnimationState.currentFrame);
    $('#animation_frame_counter').text(
      `${AnimationState.currentFrame + 1} / ${AnimationState.totalFrames}`
    );
  }

  /**
   * Update all controls UI
   */
  function updateControlsUI() {
    updatePlayButtonUI(AnimationState.isPlaying);
    updateTimelineUI();

    // Set speed dropdown
    $('#animation_speed').val(AnimationState.speed);

    // Set layer buttons
    $('.layer-toggle-btn').removeClass('active');
    $(`.layer-toggle-btn[data-layer="${AnimationState.currentLayer}"]`).addClass('active');
  }

  /**
   * Get color scale for layer
   * @param {string} layer - Layer name
   * @param {number} value - Data value
   * @returns {string} Hex color
   */
  function getColorForValue(layer, value) {
    switch (layer) {
      case 'cases':
        return getCasesColor(value);
      case 'rt':
        return getRtColor(value);
      case 'capacity':
        return getCapacityColor(value);
      default:
        return '#cccccc';
    }
  }

  function getCasesColor(cases) {
    // Color scale: low (green) to high (red)
    if (cases === 0) return '#f0f0f0';
    if (cases < 100) return '#fee5d9';
    if (cases < 500) return '#fcae91';
    if (cases < 1000) return '#fb6a4a';
    if (cases < 5000) return '#de2d26';
    return '#a50f15';
  }

  function getRtColor(rt) {
    // Color scale based on Rt thresholds
    if (rt < 0.8) return '#10B981';  // Green - declining
    if (rt < 1.0) return '#3B82F6';  // Blue - near control
    if (rt < 1.2) return '#F59E0B';  // Yellow - growing
    if (rt < 1.5) return '#EF4444';  // Orange - concerning
    return '#DC2626';  // Red - critical
  }

  function getCapacityColor(utilization) {
    // Color scale based on capacity thresholds
    if (utilization < 50) return '#10B981';  // Green
    if (utilization < 70) return '#3B82F6';  // Blue
    if (utilization < 80) return '#F59E0B';  // Yellow
    if (utilization < 90) return '#EF4444';  // Orange
    return '#DC2626';  // Red - critical
  }

  /**
   * Generate animation frames from surveillance data
   * @param {Array} data - Surveillance data
   * @param {string} dateColumn - Name of date column
   * @returns {Array} Frame data array
   */
  function generateFrames(data, dateColumn = 'observation_date') {
    // Group data by date
    const dateGroups = {};

    data.forEach(function(row) {
      const date = row[dateColumn];
      if (!dateGroups[date]) {
        dateGroups[date] = [];
      }
      dateGroups[date].push(row);
    });

    // Sort dates and create frames
    const sortedDates = Object.keys(dateGroups).sort();

    return sortedDates.map(function(date, index) {
      return {
        frame: index,
        date: date,
        data: dateGroups[date]
      };
    });
  }

  // Shiny message handlers
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('epidemicAnimation_init', function(config) {
      initializeAnimation(config);
    });

    Shiny.addCustomMessageHandler('epidemicAnimation_loadData', function(data) {
      loadFrameData(data);
    });

    Shiny.addCustomMessageHandler('epidemicAnimation_play', function(message) {
      playAnimation();
    });

    Shiny.addCustomMessageHandler('epidemicAnimation_pause', function(message) {
      pauseAnimation();
    });

    Shiny.addCustomMessageHandler('epidemicAnimation_stop', function(message) {
      stopAnimation();
    });

    Shiny.addCustomMessageHandler('epidemicAnimation_seekTo', function(frame) {
      seekToFrame(frame);
    });

    Shiny.addCustomMessageHandler('epidemicAnimation_setSpeed', function(speed) {
      setAnimationSpeed(speed);
    });

    Shiny.addCustomMessageHandler('epidemicAnimation_setLayer', function(layer) {
      setMapLayer(layer);
    });
  }

  // Expose public API
  window.RespiWatchAnimation = {
    init: initializeAnimation,
    play: playAnimation,
    pause: pauseAnimation,
    stop: stopAnimation,
    setSpeed: setAnimationSpeed,
    seekTo: seekToFrame,
    stepForward: stepForward,
    stepBackward: stepBackward,
    setLayer: setMapLayer,
    loadData: loadFrameData,
    generateFrames: generateFrames,
    getState: function() { return AnimationState; },
    getColorForValue: getColorForValue
  };

})();

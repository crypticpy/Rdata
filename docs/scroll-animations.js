/**
 * Scroll Animations for Quarto HTML Presentation
 * Apple-style reveal animations with Intersection Observer
 */

(function() {
  'use strict';

  document.addEventListener('DOMContentLoaded', function() {
    setTimeout(initScrollMode, 100);
  });

  window.addEventListener('load', function() {
    setTimeout(initScrollMode, 200);
  });

  let initialized = false;

  function initScrollMode() {
    if (initialized) return;
    initialized = true;

    console.log('Initializing Quarto scroll mode...');

    // Create progress bar
    createProgressBar();

    // Create scroll indicator
    createScrollIndicator();

    // Initialize reveal animations
    initRevealAnimations();

    // Initialize parallax effects
    initParallax();

    // Update progress bar on scroll
    window.addEventListener('scroll', updateProgressBar, { passive: true });

    // Hide scroll indicator after first scroll
    window.addEventListener('scroll', hideScrollIndicator, { once: true, passive: true });

    console.log('Scroll mode initialized');
  }

  /**
   * Create and append progress bar element
   */
  function createProgressBar() {
    const progressBar = document.createElement('div');
    progressBar.className = 'scroll-progress';
    progressBar.style.width = '0%';
    document.body.appendChild(progressBar);
  }

  /**
   * Update progress bar width based on scroll position
   */
  function updateProgressBar() {
    const scrollTop = window.scrollY;
    const docHeight = document.documentElement.scrollHeight - window.innerHeight;
    const progress = (scrollTop / docHeight) * 100;

    const progressBar = document.querySelector('.scroll-progress');
    if (progressBar) {
      progressBar.style.width = progress + '%';
    }
  }

  /**
   * Create scroll indicator at bottom of screen
   */
  function createScrollIndicator() {
    const indicator = document.createElement('div');
    indicator.className = 'scroll-indicator';
    indicator.innerHTML = `
      <svg width="30" height="50" viewBox="0 0 30 50" fill="none" xmlns="http://www.w3.org/2000/svg">
        <rect x="1" y="1" width="28" height="48" rx="14" stroke="currentColor" stroke-width="2" fill="none"/>
        <circle cx="15" cy="15" r="5" fill="currentColor">
          <animate attributeName="cy" values="15;35;15" dur="2s" repeatCount="indefinite"/>
        </circle>
      </svg>
    `;
    indicator.style.color = 'rgba(255,255,255,0.5)';
    document.body.appendChild(indicator);
  }

  /**
   * Hide scroll indicator after user scrolls
   */
  function hideScrollIndicator() {
    const indicator = document.querySelector('.scroll-indicator');
    if (indicator) {
      indicator.classList.add('hidden');
    }
  }

  /**
   * Initialize Intersection Observer for reveal animations
   */
  function initRevealAnimations() {
    // Select all elements that should animate (Quarto structure)
    const revealElements = document.querySelectorAll(
      '.section h1, ' +
      '.section h2, ' +
      '.section h3, ' +
      '.section p, ' +
      '.section li, ' +
      '.section pre, ' +
      '.section img, ' +
      '.section table, ' +
      '.section blockquote, ' +
      '.metric-card, ' +
      '.feature-card, ' +
      '.timeline-item, ' +
      '.agent-card, ' +
      '.hero-title, ' +
      '.big-quote, ' +
      '.two-column, ' +
      '.column, ' +
      '.code-block, ' +
      '.workflow-diagram, ' +
      '.cascade-diagram, ' +
      '.metrics-row, ' +
      '.agent-grid, ' +
      '.feature-grid'
    );

    // Add reveal class to elements that don't already have it
    revealElements.forEach(el => {
      if (!el.classList.contains('reveal') &&
          !el.classList.contains('hero-title') &&
          !el.classList.contains('big-quote') &&
          !el.classList.contains('metric-card') &&
          !el.classList.contains('feature-card') &&
          !el.classList.contains('timeline-item') &&
          !el.classList.contains('agent-card') &&
          !el.classList.contains('reveal-left') &&
          !el.classList.contains('reveal-right') &&
          !el.classList.contains('reveal-scale')) {
        el.classList.add('reveal');
      }
    });

    // Create observer with staggered reveals
    const observer = new IntersectionObserver((entries) => {
      entries.forEach((entry, index) => {
        if (entry.isIntersecting) {
          // Add small delay for staggered effect within viewport
          const delay = Math.min(index * 50, 300);
          setTimeout(() => {
            entry.target.classList.add('visible');
          }, delay);

          // Unobserve after revealing (one-time animation)
          observer.unobserve(entry.target);
        }
      });
    }, {
      root: null,
      rootMargin: '-10% 0px -10% 0px',
      threshold: 0.1
    });

    // Observe all reveal elements
    document.querySelectorAll('.reveal, .reveal-left, .reveal-right, .reveal-scale, .hero-title, .big-quote, .metric-card, .feature-card, .timeline-item, .agent-card').forEach(el => {
      observer.observe(el);
    });

    // Also observe sections for section-level animations
    const sectionObserver = new IntersectionObserver((entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          entry.target.classList.add('visible');
        }
      });
    }, {
      threshold: 0.2
    });

    document.querySelectorAll('.section').forEach(section => {
      sectionObserver.observe(section);
    });
  }

  /**
   * Initialize parallax effects for images
   */
  function initParallax() {
    const parallaxImages = document.querySelectorAll('.parallax-img, .section img:not(.no-parallax)');

    if (parallaxImages.length === 0) return;

    let ticking = false;

    window.addEventListener('scroll', () => {
      if (!ticking) {
        requestAnimationFrame(() => {
          parallaxImages.forEach(img => {
            const rect = img.getBoundingClientRect();

            // Only apply parallax when image is in viewport
            if (rect.top < window.innerHeight && rect.bottom > 0) {
              const yPos = -(rect.top - window.innerHeight / 2) * 0.05;
              img.style.transform = `translateY(${yPos}px)`;
            }
          });
          ticking = false;
        });
        ticking = true;
      }
    }, { passive: true });
  }

  /**
   * Counter animation for metric numbers
   */
  window.animateCounter = function(element, target, duration = 2000) {
    const start = 0;
    const startTime = performance.now();

    function update(currentTime) {
      const elapsed = currentTime - startTime;
      const progress = Math.min(elapsed / duration, 1);

      // Easing function (ease-out-cubic)
      const easeProgress = 1 - Math.pow(1 - progress, 3);

      const current = Math.round(start + (target - start) * easeProgress);
      element.textContent = current.toLocaleString();

      if (progress < 1) {
        requestAnimationFrame(update);
      }
    }

    requestAnimationFrame(update);
  };

  /**
   * Typewriter effect for text
   */
  window.typewriterEffect = function(element, text, speed = 50) {
    element.textContent = '';
    let i = 0;

    function type() {
      if (i < text.length) {
        element.textContent += text.charAt(i);
        i++;
        setTimeout(type, speed);
      }
    }

    type();
  };

})();

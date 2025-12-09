/**
 * Slide Controller for Button-Based Navigation
 * Replaces scroll-snap with precise slide control
 */

(function() {
  'use strict';

  let currentSlide = 0;
  let slides = [];
  let totalSlides = 0;
  let initialized = false;

  document.addEventListener('DOMContentLoaded', function() {
    setTimeout(initSlideController, 100);
  });

  window.addEventListener('load', function() {
    setTimeout(initSlideController, 200);
  });

  function initSlideController() {
    if (initialized) return;
    initialized = true;

    slides = document.querySelectorAll('.slide');
    totalSlides = slides.length;

    if (totalSlides === 0) {
      console.warn('No slides found. Make sure elements have class "slide"');
      return;
    }

    console.log(`Slide controller initialized with ${totalSlides} slides`);

    // Create navigation UI
    createNavButtons();
    createProgressBar();

    // Show first slide
    showSlide(0);

    // Keyboard navigation
    document.addEventListener('keydown', handleKeyboard);

    // Touch/swipe support
    initTouchSupport();
  }

  function showSlide(index) {
    if (index < 0 || index >= totalSlides) return;

    slides.forEach((slide, i) => {
      slide.classList.remove('active', 'slide-enter', 'slide-exit');

      if (i === index) {
        slide.classList.add('active');
        // Small delay for CSS transition to work
        requestAnimationFrame(() => {
          slide.classList.add('slide-enter');
          triggerAnimations(slide);
        });
      }
    });

    currentSlide = index;
    updateProgress();
    updateCounter();
  }

  function nextSlide() {
    if (currentSlide < totalSlides - 1) {
      showSlide(currentSlide + 1);
    }
  }

  function prevSlide() {
    if (currentSlide > 0) {
      showSlide(currentSlide - 1);
    }
  }

  function goToSlide(index) {
    showSlide(index);
  }

  function triggerAnimations(slide) {
    // Reset animations first
    const animElements = slide.querySelectorAll(
      '.reveal, .reveal-left, .reveal-right, .reveal-scale, ' +
      '.timeline-item, .agent-card, .feature-card, .metric-card, ' +
      '.pipeline-stage, .pipeline-arrow, .journey-step, ' +
      '.fallback-source, .fallback-arrow, .fallback-label, ' +
      '.api-group, .capability-card, ' +
      '.hero-title, .big-quote, .column, h1, h2, h3, p, li, blockquote, pre'
    );

    animElements.forEach((el) => {
      el.classList.remove('visible', 'animate-in');
    });

    // Handle timeline items specially - much longer delays (1.5s between each)
    const timelineItems = slide.querySelectorAll('.timeline-item');
    if (timelineItems.length > 0) {
      timelineItems.forEach((item, i) => {
        setTimeout(() => {
          item.classList.add('visible', 'animate-in');
        }, i * 1500); // 1.5 seconds between each day
      });
    }

    // Handle agent cards with staggered delays (300ms between each)
    const agentCards = slide.querySelectorAll('.agent-card');
    if (agentCards.length > 0) {
      agentCards.forEach((card, i) => {
        setTimeout(() => {
          card.classList.add('visible', 'animate-in');
        }, i * 300); // 300ms between each card
      });
    }

    // Handle pipeline stages with staggered delays (400ms between each)
    const pipelineStages = slide.querySelectorAll('.pipeline-stage, .pipeline-arrow');
    if (pipelineStages.length > 0) {
      pipelineStages.forEach((stage, i) => {
        setTimeout(() => {
          stage.classList.add('visible', 'animate-in');
        }, i * 400); // 400ms between each stage/arrow
      });
    }

    // Handle journey steps (discovery story) with staggered delays (600ms between each)
    const journeySteps = slide.querySelectorAll('.journey-step');
    if (journeySteps.length > 0) {
      journeySteps.forEach((step, i) => {
        setTimeout(() => {
          step.classList.add('visible', 'animate-in');
        }, i * 600); // 600ms between each step
      });
    }

    // Handle fallback flowchart elements with staggered delays (500ms between each)
    const fallbackElements = slide.querySelectorAll('.fallback-source, .fallback-arrow, .fallback-label');
    if (fallbackElements.length > 0) {
      fallbackElements.forEach((el, i) => {
        setTimeout(() => {
          el.classList.add('visible', 'animate-in');
        }, i * 300); // 300ms between each element
      });
    }

    // Handle API groups with staggered delays (400ms between each)
    const apiGroups = slide.querySelectorAll('.api-group');
    if (apiGroups.length > 0) {
      apiGroups.forEach((group, i) => {
        setTimeout(() => {
          group.classList.add('visible', 'animate-in');
        }, i * 400); // 400ms between each group
      });
    }

    // Handle capability cards with staggered delays (250ms between each)
    const capabilityCards = slide.querySelectorAll('.capability-card');
    if (capabilityCards.length > 0) {
      capabilityCards.forEach((card, i) => {
        setTimeout(() => {
          card.classList.add('visible', 'animate-in');
        }, i * 250); // 250ms between each card
      });
    }

    // Handle other elements with standard stagger
    animElements.forEach((el, i) => {
      // Skip specially handled elements
      if (el.classList.contains('timeline-item') ||
          el.classList.contains('agent-card') ||
          el.classList.contains('pipeline-stage') ||
          el.classList.contains('pipeline-arrow') ||
          el.classList.contains('journey-step') ||
          el.classList.contains('fallback-source') ||
          el.classList.contains('fallback-arrow') ||
          el.classList.contains('fallback-label') ||
          el.classList.contains('api-group') ||
          el.classList.contains('capability-card')) return;

      const delay = Math.min(i * 80, 600);
      setTimeout(() => {
        el.classList.add('visible', 'animate-in');
      }, delay);
    });
  }

  function handleKeyboard(e) {
    // Don't handle if user is typing in an input
    if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA') return;

    switch(e.key) {
      case 'ArrowRight':
      case ' ':
      case 'PageDown':
        e.preventDefault();
        nextSlide();
        break;
      case 'ArrowLeft':
      case 'PageUp':
        e.preventDefault();
        prevSlide();
        break;
      case 'Home':
        e.preventDefault();
        goToSlide(0);
        break;
      case 'End':
        e.preventDefault();
        goToSlide(totalSlides - 1);
        break;
    }
  }

  function createNavButtons() {
    const nav = document.createElement('div');
    nav.className = 'slide-nav';
    nav.innerHTML = `
      <button class="nav-btn prev" aria-label="Previous slide">
        <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
          <polyline points="15,18 9,12 15,6"></polyline>
        </svg>
      </button>
      <span class="slide-counter">
        <span id="slide-current">1</span>
        <span class="counter-sep">/</span>
        <span id="slide-total">${totalSlides}</span>
      </span>
      <button class="nav-btn next" aria-label="Next slide">
        <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
          <polyline points="9,6 15,12 9,18"></polyline>
        </svg>
      </button>
    `;
    document.body.appendChild(nav);

    // Attach event listeners
    nav.querySelector('.prev').addEventListener('click', prevSlide);
    nav.querySelector('.next').addEventListener('click', nextSlide);
  }

  function createProgressBar() {
    const progressBar = document.createElement('div');
    progressBar.className = 'slide-progress';
    progressBar.innerHTML = '<div class="slide-progress-fill"></div>';
    document.body.appendChild(progressBar);
  }

  function updateProgress() {
    const fill = document.querySelector('.slide-progress-fill');
    if (fill) {
      const progress = ((currentSlide + 1) / totalSlides) * 100;
      fill.style.width = `${progress}%`;
    }
  }

  function updateCounter() {
    const currentEl = document.getElementById('slide-current');
    if (currentEl) {
      currentEl.textContent = currentSlide + 1;
    }
  }

  function initTouchSupport() {
    let touchStartX = 0;
    let touchEndX = 0;

    document.addEventListener('touchstart', (e) => {
      touchStartX = e.changedTouches[0].screenX;
    }, { passive: true });

    document.addEventListener('touchend', (e) => {
      touchEndX = e.changedTouches[0].screenX;
      handleSwipe();
    }, { passive: true });

    function handleSwipe() {
      const swipeThreshold = 50;
      const diff = touchStartX - touchEndX;

      if (Math.abs(diff) > swipeThreshold) {
        if (diff > 0) {
          nextSlide(); // Swipe left = next
        } else {
          prevSlide(); // Swipe right = prev
        }
      }
    }
  }

  // Expose functions globally for onclick handlers
  window.nextSlide = nextSlide;
  window.prevSlide = prevSlide;
  window.goToSlide = goToSlide;

})();

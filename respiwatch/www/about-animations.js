// =============================================================================
// About Page Animations - Stats Counter Animation
// Purpose: Animates stat numbers counting up when section scrolls into view
// =============================================================================

document.addEventListener('DOMContentLoaded', function() {
  const statsHighlight = document.querySelector('.stats-highlight');
  if (!statsHighlight) return;

  // Flag to ensure animation only runs once
  let hasAnimated = false;

  const observer = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (entry.isIntersecting && !hasAnimated) {
        hasAnimated = true;
        animateStats();
        observer.unobserve(entry.target);
      }
    });
  }, { threshold: 0.5 });

  observer.observe(statsHighlight);

  function animateStats() {
    const statValues = document.querySelectorAll('.stat-value');

    statValues.forEach(stat => {
      const finalValue = stat.textContent.trim();

      // Extract numeric part, prefix (like ~), and suffix (like + or %)
      const numericMatch = finalValue.match(/(\d+)/);
      if (!numericMatch) return;

      const numericPart = parseInt(numericMatch[1]);
      const prefix = finalValue.match(/^[~<>]/) ? finalValue.match(/^[~<>]/)[0] : '';
      const suffix = finalValue.match(/[+%]$/) ? finalValue.match(/[+%]$/)[0] : '';

      // Start from 0
      let current = 0;
      const duration = 1500; // Animation duration in ms
      const steps = 30;
      const increment = numericPart / steps;
      const stepDuration = duration / steps;

      // Store original to restore if needed
      const originalValue = finalValue;

      const timer = setInterval(() => {
        current += increment;

        if (current >= numericPart) {
          stat.textContent = originalValue;
          clearInterval(timer);
        } else {
          stat.textContent = prefix + Math.floor(current) + suffix;
        }
      }, stepDuration);
    });
  }
});

/**
 * RespiWatch: Code Transparency JavaScript
 * Copy-to-clipboard and code modal interactions
 */

// Copy code to clipboard
async function copyCodeToClipboard(button, codeId) {
  const codeElement = document.getElementById(codeId);
  if (!codeElement) {
    console.error('Code element not found:', codeId);
    return;
  }

  const codeText = codeElement.textContent || codeElement.innerText;

  try {
    await navigator.clipboard.writeText(codeText);

    // Visual feedback - change button text
    const originalHTML = button.innerHTML;
    button.innerHTML = '<i class="fas fa-check"></i> Copied!';
    button.classList.add('copied');

    // Reset after 2 seconds
    setTimeout(() => {
      button.innerHTML = originalHTML;
      button.classList.remove('copied');
    }, 2000);

  } catch (err) {
    // Fallback for older browsers
    const textArea = document.createElement('textarea');
    textArea.value = codeText;
    textArea.style.position = 'fixed';
    textArea.style.left = '-9999px';
    document.body.appendChild(textArea);
    textArea.select();

    try {
      document.execCommand('copy');
      button.innerHTML = '<i class="fas fa-check"></i> Copied!';
      button.classList.add('copied');
      setTimeout(() => {
        button.innerHTML = '<i class="fas fa-copy"></i> Copy';
        button.classList.remove('copied');
      }, 2000);
    } catch (e) {
      console.error('Copy failed:', e);
      button.innerHTML = '<i class="fas fa-times"></i> Failed';
      setTimeout(() => {
        button.innerHTML = '<i class="fas fa-copy"></i> Copy';
      }, 2000);
    }

    document.body.removeChild(textArea);
  }
}

// Add modal class when code modal opens
$(document).on('show.bs.modal', '.modal', function() {
  // Check if this is a code modal (has code-section inside)
  if ($(this).find('.code-section').length > 0) {
    $(this).addClass('code-modal');
  }
});

// Initialize code block interactions
$(document).ready(function() {
  // Add smooth scrolling to code blocks
  $(document).on('mouseenter', '.code-content', function() {
    $(this).css('scroll-behavior', 'smooth');
  });
});

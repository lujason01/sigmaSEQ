// GEO Data page handlers
$(document).ready(function() {
    // Update platform selection when GEO data is loaded
    Shiny.addCustomMessageHandler("updatePlatformChoices", function(data) {
        let selectInput = $("#" + data.id);
        selectInput.empty();
        data.choices.forEach(function(platform) {
            selectInput.append(new Option(platform, platform));
        });
    });

    // Update sample selection when platform is selected
    Shiny.addCustomMessageHandler("updateSampleChoices", function(data) {
        let selectInput = $("#" + data.id);
        selectInput.empty();
        data.choices.forEach(function(sample) {
            selectInput.append(new Option(sample, sample));
        });
    });

    // Show/hide loading container with bs4Dash integration
    Shiny.addCustomMessageHandler("toggleLoading", function(message) {
        const loadingContainer = document.getElementById(message.id);
        if (loadingContainer) {
            // Use bs4Dash loading overlay if available
            if (typeof AdminLTE !== 'undefined') {
                if (message.show) {
                    $(loadingContainer).closest('.card').LoadingOverlay("show");
                } else {
                    $(loadingContainer).closest('.card').LoadingOverlay("hide");
                }
            } else {
                // Fallback to basic display toggle
                loadingContainer.style.display = message.show ? "block" : "none";
                
                // Toggle spinner visibility
                const spinner = loadingContainer.querySelector(".geo-spinner");
                if (spinner) {
                    spinner.style.display = message.show ? "inline-block" : "none";
                }
            }
        }
    });

    // Update progress bar with bs4Dash integration
    Shiny.addCustomMessageHandler("updateProgress", function(message) {
        const progressBar = document.getElementById(message.id);
        const progressText = document.getElementById(message.textId);
        
        if (progressBar && progressText) {
            // Use bs4Dash progress bar if available
            if (typeof AdminLTE !== 'undefined') {
                $(progressBar).closest('.progress-bar')
                    .css('width', message.value + '%')
                    .attr('aria-valuenow', message.value);
            } else {
                // Fallback to basic progress bar
                progressBar.style.width = message.value + "%";
            }
            
            // Update text
            progressText.innerText = message.text || message.value + "%";
        }
    });

    // Show status message with bs4Dash toast integration
    Shiny.addCustomMessageHandler("showStatus", function(message) {
        // Use bs4Dash toast if available
        if (typeof AdminLTE !== 'undefined') {
            const toastType = {
                'success': 'success',
                'error': 'danger',
                'info': 'info',
                'warning': 'warning'
            }[message.type] || 'info';
            
            $(document).Toasts('create', {
                title: 'GEO Data Status',
                body: message.text,
                class: `bg-${toastType}`,
                autohide: message.type === 'success' || message.type === 'info',
                delay: 5000
            });
        } else {
            // Fallback to basic status container
            const container = document.getElementById(message.id);
            if (container) {
                container.classList.remove("geo-info", "geo-success", "geo-warning", "geo-error");
                container.classList.add("geo-" + message.type);
                container.innerText = message.text;
                container.style.display = "block";
                
                if (message.type === "success" || message.type === "info") {
                    setTimeout(function() {
                        container.style.display = "none";
                    }, 5000);
                }
            }
        }
    });

    // Toggle process button visibility
    Shiny.addCustomMessageHandler("toggleProcessButton", function(message) {
        const button = document.getElementById(message.id);
        if (button) {
            $(button).fadeToggle(message.show);
        }
    });

    // Helper function to show status messages
    function showStatus(containerId, text, type) {
        const container = document.getElementById(containerId + "-status_container");
        if (container) {
            // Remove all existing status classes
            container.classList.remove("geo-info", "geo-success", "geo-warning", "geo-error");
            // Add the new status class
            container.classList.add("geo-" + type);
            container.innerText = text;
            container.style.display = "block";
            
            // Auto-hide success/info messages after 5 seconds
            if (type === "success" || type === "info") {
                setTimeout(function() {
                    container.style.display = "none";
                }, 5000);
            }
        }
    }

    // Handle platform change
    $(document).on('change', 'select[id$="platform"]', function() {
        const ns = $(this).closest('.geo-container').attr('id').replace('-container', '');
        showStatus(ns, 'Loading samples for selected platform...', 'info');
    });
    
    // Handle sample selection
    $(document).on('change', 'select[id$="samples"]', function() {
        const selectedCount = $(this).val() ? $(this).val().length : 0;
        const ns = $(this).closest('.geo-container').attr('id').replace('-container', '');
        
        if (selectedCount > 0) {
            showStatus(ns, selectedCount + ' samples selected', 'info');
        } else {
            showStatus(ns, 'Please select at least one sample', 'warning');
        }
    });
    
    // Initialize components
    $('.geo-spinner').css({
        'display': 'none',
        'position': 'absolute',
        'top': '50%',
        'left': '50%',
        'transform': 'translate(-50%, -50%)'
    });
    $('.geo-progress-bar').css('width', '0%');
    
    $('.geo-loading-container').css({
        'position': 'relative',
        'min-height': '100px',
        'background-color': 'rgba(255, 255, 255, 0.8)'
    });
    
    // Export functions for use in Shiny
    window.GEOHandlers = {
        showStatus: showStatus,
        updateProgress: function(id, value, text) {
            const message = { id: id, value: value, text: text };
            Shiny.addCustomMessageHandler("updateProgress")(message);
        },
        toggleLoading: function(id, show) {
            const message = { id: id, show: show };
            Shiny.addCustomMessageHandler("toggleLoading")(message);
        }
    };
}); 
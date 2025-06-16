$(document).ready(function() {
  // Add custom JavaScript handlers
  $('.box').on('click', '.box-header h3', function() {
    $(this).closest('.box').find('.box-body').slideToggle();
  });
});

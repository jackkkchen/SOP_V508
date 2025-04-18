$(document).ready(function() {
  // Initialize font size
  var currentFontSize = 100;
  
  // Update font size function
  function updateFontSize(size) {
    currentFontSize = size;
    document.documentElement.style.fontSize = size + "%";
    $("#font_size").val(size);
    $(".irs-single").text(size + "%");
  }
  
  // Keyboard shortcuts
  $(document).on("keydown", function(e) {
    if (e.altKey) {
      if (e.key.toLowerCase() === "t") {
        e.preventDefault();
        $("#dark_mode").trigger("click");
      } else if (e.key.toLowerCase() === "h") {
        e.preventDefault();
        $("#contrast_toggle").trigger("click");
      } else if (e.key === "ArrowUp") {
        e.preventDefault();
        var newSize = Math.min(currentFontSize + 10, 150);
        updateFontSize(newSize);
        $("#font_size").trigger("change");
      } else if (e.key === "ArrowDown") {
        e.preventDefault();
        var newSize = Math.max(currentFontSize - 10, 80);
        updateFontSize(newSize);
        $("#font_size").trigger("change");
      }
    }
  });
  
  // Listen for slider changes
  $("#font_size").on("change", function() {
    var size = parseInt($(this).val()) || 100;
    updateFontSize(size);
  });
}); 
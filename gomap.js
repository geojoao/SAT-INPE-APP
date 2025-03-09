// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
    e.preventDefault();
    $el = $(this);
    var lat = $el.data("lat");
    var long = $el.data("long");
    var zoom = $el.data("zoom");
    
    $("#map").leaflet().setView([lat, long], zoom);
}); 
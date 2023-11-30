$(document).ready(function() {
  $('#datatablesSimple').dataTable( {
    ajax: {
      url : '/ajax.json/'
    },
    processing: true,
    serverSide: true    
  }); 
});
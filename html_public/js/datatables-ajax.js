$(document).ready(function() {
  $('#datatablesSimple').dataTable( {
    "lengthMenu": [ [25, 50, 100, 300], [25, 50, 100, 300] ],
    ajax: {
      url : '/ajax.json/'
    },
    processing: true,
    serverSide: true    
  }); 
});
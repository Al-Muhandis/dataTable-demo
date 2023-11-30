$(document).ready(function() {
  $('#datatablesSimple').dataTable( {
    ajax: {
      url : '/ajax.json/'
    },
    processing: true,
    serverSide: true    
  });
  $('#updatebtn').click(function() { 
  
       table = $('#datatablesSimple').dataTable({
           destroy: true,
           ajax: {
             url : '/ajax.json/'
           },
           processing: true,
           serverSide: true          
       });
       table.ajax.reload();
  }); 
  $('#clearbtn').click(function() {
       table = $('#datatablesSimple').DataTable({
           destroy: true,
           serverSide: false          
       });
       table.clear().draw();
    }); 
});
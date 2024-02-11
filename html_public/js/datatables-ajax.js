$(document).ready(function() {
  var table=$('#datatablesSimple').DataTable( {
    ajax: {
      url : '/ajax.json/tableread/'
    },
    processing: true,
    serverSide: true    
  });
  function myCallbackFunction (updatedCell, updatedRow, oldValue) {
    var rowID = updatedRow.data()[0];
    var cellValue = updatedCell.data();
    var cellColumn = updatedCell.index().column;
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/ajax.json/celledit/", true);
    xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");

    var data = 'id='+rowID+'&'+'cellvalue='+cellValue+'&'+'column='+cellColumn;

    xhr.onreadystatechange = function() {
        if (xhr.readyState === XMLHttpRequest.DONE) {
            if (xhr.status === 200) {
                console.log("Данные успешно отправлены на сервер");
            } else {
                console.error("Произошла ошибка при отправке данных на сервер");
            }
        }
    };

    xhr.send(data);
  };
  table.MakeCellsEditable({
      "onUpdate": myCallbackFunction,
      "confirmationButton": true
  });
  $('#updatebtn').click(function() { 
  
       table = $('#datatablesSimple').dataTable({
           destroy: true,
           ajax: {
             url : '/ajax.json/tableread/'
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
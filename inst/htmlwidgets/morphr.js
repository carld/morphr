
// need variables methods, server, table, function tweakCellIndex

function morphr_renderValue(methods, server, table, tweakCellIndex) {

  var myFindIndex = function(cell, cells) {
    for (var i = 0; i < cells.length; i++) {
      if (cell[0] === cells[i][0] && cell[1] === cells[i][1]) return i;
    }
    return -1;
  };

  var consistentClass = 'consistent';
  methods.setCellsConsistent = function(consistent) {
    consistent = consistent ? consistent : [];
    //console.log("consistent: " + consistent.toString());
    table.$('td.' + consistentClass).removeClass(consistentClass);
    if (consistent.length === 0) return;
    if (server) {
      table.cells({page: 'current'}).every(function() {
        var info = tweakCellIndex(this);
        //console.log("for loop is at cell: ")
        //console.log(info);
        //console.log("this is cell at consistent index: " + myFindIndex([info.row, info.col], consistent));
        if (myFindIndex([info.row, info.col], consistent) > -1)
          $(this.node()).addClass(consistentClass);
      });
    } else {
      consistent.map(function(ij) {
        $(table.cell(ij[0] - 1, ij[1]).node()).addClass(consistentClass);
      });
    }
  };

}

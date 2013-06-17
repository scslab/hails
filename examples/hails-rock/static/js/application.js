$(document).ready(function() {
  $("#newGame").submit(function() {
    if ($("#opponent").val() == "") {
      $("#opponent").attr("disabled",true);
    }
  });
});

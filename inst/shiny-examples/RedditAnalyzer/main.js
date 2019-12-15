var checked = $(".cbox");

checked.click(function() {
  if (checked.prop("checked")) {
    $(".add").text("Hit Enter to Submit");
  }

  if (!checked.prop("checked")) {
    $(".message").val("");
    $(".add").text("Add Comment");
  }
});


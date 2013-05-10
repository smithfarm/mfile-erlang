"use strict";

MFILE.testLDAP = function () {
   $('#mainarea').html(MFILE.html.test_ldap1);
   MFILE.testLDAPajax();
   $("#mainarea").append(MFILE.html.press_any_key);
   $("#continue").focus();
   $("#continue").keydown(function(event) {
      event.preventDefault();
      MFILE.state = "MAIN_MENU";
      MFILE.actOnState();
   });
}

MFILE.testLDAPajax = function() {
   console.log("About to test LDAP");
   $.ajax({
      url: "testldap",
      type: "GET",
   });
}

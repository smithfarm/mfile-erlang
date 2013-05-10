"use strict";

MFILE.cookie = {};

MFILE.cookie.view = function () {
   $('#mainarea').html(MFILE.html.view_cookie1);
   $('#mainarea').append(document.cookie);
   $("#mainarea").append(MFILE.html.press_any_key);
   $("#continue").focus();
   $("#continue").keydown(function(event) {
      event.preventDefault();
      MFILE.state = "MAIN_MENU";
      MFILE.actOnState();
   });
}


// cookie functions (by Scott Andrew)

MFILE.cookie.create = function (name,value,days) {
	if (days) {
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

MFILE.cookie.read = function (name) {
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}

MFILE.cookie.erase = function (name) {
   MFILE.cookie.create(name,"",-1);
}


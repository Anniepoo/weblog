$(window).load(function(){
	var footnoteactivator = $('.fn');
	footnoteactivator.mouseenter(function() {
		$(this).find('span.fnp').show(100);
	});
	footnoteactivator.mouseleave(function() {
		var t = $(this).find('span.fnp');
		window.setTimeout(
			
			function() {
				  t.hide(100);
			}, 3000);
	});
});
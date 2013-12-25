$(window).load(function(){
	var footnoteactivator = $('.fn');
	footnoteactivator.mouseenter(function() {
		window.clearTimeout(this.footnoteid);
		$(this).find('span.fnp').show(100);
	});
	footnoteactivator.mouseleave(function() {
		var t = $(this).find('span.fnp');
		if(this.footnoteid !== null)
		{
		    window.clearTimeout(this.footnoteid);
		}

		this.footnoteid = window.setTimeout(
			
			function() {
				t.hide(100);
			}, 3000);  
	});
});
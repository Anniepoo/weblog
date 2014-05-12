// wlajaxify.js
// weblog ajax library
function ajaxify_object() {
	this.listen = function(name, id, uri) {
		listeners.push(
			new wl_listener(name, id, uri)
		);
	}
	
	this.tick = function(speed, id, uri) {
		setInterval("ajaxify.talk(\"" + id + "\")", speed);
	}
	
	this.add_contents = function(message_id, dom_id) {
		wl_args.push(
			new wl_argset(message_id, dom_id)
		);
	}
	
	this.get_args = function(id) {
		var ret = "";
		var i;
		
		for(i = 0 ; i < wl_args.length ; i++) {
			if(wl_args[i].message_id == id) {
				if(ret == "") {
					ret = "?" + wl_args[i].dom_id +
					      "=" + encodeURIComponent($("#" + wl_args[i].dom_id).val());
				} else {
					ret = ret + "&" + wl_args[i].dom_id +
					      "=" + encodeURIComponent($("#" + wl_args[i].dom_id).val());
				}
			}
		}
		
		return ret;
	}
	
	this.talk = function(xid) {
		var i;
		for(i = 0 ; i < listeners.length ; i++) {
			if(listeners[i].id == xid) {
				$.ajax({
				   url: listeners[i].uri + this.get_args(xid),
				   success: function(data, status, xhr) {
					 var clear   = xhr.getResponseHeader("X-Clear");
					 var id      = xhr.getResponseHeader("X-Id");
					 var timeout = xhr.getResponseHeader("X-Timeout");

					 if ( !timeout )
					 { 
						if ( clear ) { 
						  $("#"+id).empty();
						}
						$("#"+id).append(data);
						$("#error").empty();
					 }
				   },
				   error: function(jqXHDR, why, error) {
					 { $("#error").empty();
					   $("#error").append('<h4 class="error">Error:</h4>');
					   if ( jqXHDR.status )
					   { $("#error").append("Status:" + jqXHDR.status);
					 $("#error").append(jqXHDR.responseText);
					 read_messages();
					   } else
					   { $("#error").append("Lost connection; stopped!");
					   }
					 }
				   }
				 });
			 }
		}
	}
}

var ajaxify = new ajaxify_object();

var listeners = new Array(0);

var wl_args = new Array(0);

function wl_listener(cname, cid, curi) {
	this.name = cname;
	this.id = cid;
	this.uri = curi;
}

function wl_argset(cmessage_id, cdom_id) {
	this.message_id = cmessage_id;
	this.dom_id = cdom_id;
}



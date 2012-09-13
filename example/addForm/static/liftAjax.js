(function() {

    window.liftAjax = {
        lift_ajaxQueue: [],
        lift_ajaxInProcess: null,
        lift_doCycleQueueCnt: 0,
        lift_ajaxShowing: false,
        lift_ajaxRetryCount: 3,

        lift_ajaxHandler: function(theData, theSuccess, theFailure, responseType){
            var toSend = {retryCnt: 0};
            toSend.when = (new Date()).getTime();
            toSend.theData = theData;
            toSend.onSuccess = theSuccess;
            toSend.onFailure = theFailure;
            toSend.responseType = responseType;

            liftAjax.lift_ajaxQueue.push(toSend);
            liftAjax.lift_ajaxQueueSort();
            liftAjax.lift_doCycleQueueCnt++;
            liftAjax.lift_doAjaxCycle();
            return false; // buttons in forms don't trigger the form

        },

        lift_uriSuffix: undefined,

        lift_ajaxQueueSort: function() {
            liftAjax.lift_ajaxQueue.sort(function (a, b) {return a.when - b.when;});
        },

        lift_defaultFailure: function() {
            alert("The server cannot be contacted at this time.  Try reloading the page.");
        },

        lift_startAjax: function() {
            liftAjax.lift_ajaxShowing = true;
            jQuery('#'+"ajax-loader").show();
        },

        lift_endAjax: function() {
            liftAjax.lift_ajaxShowing = false;
            jQuery('#'+"ajax-loader").hide();
        },

        lift_testAndShowAjax: function() {
            if (liftAjax.lift_ajaxShowing && liftAjax.lift_ajaxQueue.length == 0 && liftAjax.lift_ajaxInProcess == null) {
                liftAjax.lift_endAjax();
            } else if (!liftAjax.lift_ajaxShowing && (liftAjax.lift_ajaxQueue.length > 0 || liftAjax.lift_ajaxInProcess != null)) {
                liftAjax.lift_startAjax();
            }
        },

        lift_traverseAndCall: function(node, func) {
            if (node.nodeType == 1) func(node);
            var i = 0;
            var cn = node.childNodes;

            for (i = 0; i < cn.length; i++) {
                liftAjax.lift_traverseAndCall(cn.item(i), func);
            }
        },

        lift_successRegisterGC: function() {
            setTimeout("liftAjax.lift_registerGC()", 75000);
        },

        lift_failRegisterGC: function() {
            setTimeout("liftAjax.lift_registerGC()", 15000);
        },

        lift_registerGC: function() {
      var data = "__lift__GC=_"
            jQuery.ajax({ url : liftAjax.addPageName("/" + lift_ajaxUrl + "/request/"), data : data, type : "POST", dataType : "script", timeout : 5000, cache : false, success : liftAjax.lift_successRegisterGC, error : liftAjax.lift_failRegisterGC });
        },

        lift_doAjaxCycle: function() {
            if (liftAjax.lift_doCycleQueueCnt > 0) liftAjax.lift_doCycleQueueCnt--;
            var queue = liftAjax.lift_ajaxQueue;
            if (queue.length > 0) {
                var now = (new Date()).getTime();
                if (liftAjax.lift_ajaxInProcess == null && queue[0].when <= now) {
                    var aboutToSend = queue.shift();

                    liftAjax.lift_ajaxInProcess = aboutToSend;

                    var successFunc = function(data) {
                        liftAjax.lift_ajaxInProcess = null;
                        if (aboutToSend.onSuccess) {
                            aboutToSend.onSuccess(data);
                        }
                        liftAjax.lift_doCycleQueueCnt++;
                        liftAjax.lift_doAjaxCycle();
                    };

                    var failureFunc = function() {
                        liftAjax.lift_ajaxInProcess = null;
                        var cnt = aboutToSend.retryCnt;
                        if (cnt < liftAjax.lift_ajaxRetryCount) {
                            aboutToSend.retryCnt = cnt + 1;
                            var now = (new Date()).getTime();
                            aboutToSend.when = now + (1000 * Math.pow(2, cnt));
                            queue.push(aboutToSend);
                            liftAjax.lift_ajaxQueueSort();
                        } else {
                            if (aboutToSend.onFailure) {
                                aboutToSend.onFailure();
                            } else {
                                liftAjax.lift_defaultFailure();
                            }
                        }
                        liftAjax.lift_doCycleQueueCnt++;
                        liftAjax.lift_doAjaxCycle();
                    };

                    if (aboutToSend.responseType != undefined &&
                 aboutToSend.responseType != null &&
                        aboutToSend.responseType.toLowerCase() === "json") {
                        liftAjax.lift_actualJSONCall(aboutToSend.theData, successFunc, failureFunc);
                    } else {
                        var theData = aboutToSend.theData;
                        if (liftAjax.lift_uriSuffix) {
                            theData += '&' + liftAjax.lift_uriSuffix;
                            liftAjax.lift_uriSuffix = undefined;
                        }
                        liftAjax.lift_actualAjaxCall(theData, successFunc, failureFunc);
                    }
                }
            }

            liftAjax.lift_testAndShowAjax();
            if (liftAjax.lift_doCycleQueueCnt <= 0) liftAjax.lift_doCycleIn200()
        },

        lift_doCycleIn200: function() {
            liftAjax.lift_doCycleQueueCnt++;
            setTimeout("liftAjax.lift_doAjaxCycle();", 200);
        },

        addPageName: function(url) {
            return url.replace(lift_ajaxUrl + '/request', lift_ajaxUrl + '/request/'+lift_page);
        },

        lift_actualAjaxCall: function(data, onSuccess, onFailure) {
            jQuery.ajax({ url : liftAjax.addPageName("/" + lift_ajaxUrl + "/request/"), data : data, type : "POST", dataType : "script", timeout : 5000, cache : false, success : onSuccess, error : onFailure });
        },

        lift_actualJSONCall: function(data, onSuccess, onFailure) {
            jQuery.ajax({ url : liftAjax.addPageName("/" + lift_ajaxUrl + "/request/"), data : data, type : "POST", dataType : "json", timeout : 5000, cache : false, success : onSuccess, error : onFailure });
        }
    };

    window.liftUtils = {
        lift_blurIfReturn: function(e) {
            var code;
            if (!e) var e = window.event;
            if (e.keyCode) code = e.keyCode;
            else if (e.which) code = e.which;

            var targ;

            if (e.target) targ = e.target;
            else if (e.srcElement) targ = e.srcElement;
            if (targ.nodeType == 3) // defeat Safari bug
                targ = targ.parentNode;
            if (code == 13) {targ.blur(); return false;} else {return true;};
        }
    };


})();
jQuery(document).ready(function() {liftAjax.lift_doCycleIn200();});